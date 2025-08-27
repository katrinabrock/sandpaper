# The functions in this file will run after all the content from the markdown
# files have been written to HTML. This will allow us to extract rendered
# content from these files to aggregate into summary files such as instructor
# notes, glossaries, and all-in-one page.

#' read all HTML files in a folder
#'
#' @param path the path to a folder with HTML files
#' @return a nested list of `html_documents` from [xml2::read_html()] with two
#'   top-level elements:
#'   - `$learner`: contains all of the html pages in the learner view
#'   - `$instructor`: all of the pages in the instructor view
#'   - `$paths`: the absolute paths for the pages
#'
#' @keywords internal
#' @examples
#' tmpdir <- tempfile()
#' on.exit(fs::dir_delete(tmpdir))
#' fs::dir_create(tmpdir)
#' fs::dir_create(fs::path(tmpdir, "instructor"))
#' writeLines("<p>Instructor</p>", fs::path(tmpdir, "instructor", "index.html"))
#' writeLines("<p>Learner</p>", fs::path(tmpdir, "index.html"))
#' sandpaper:::read_all_html(tmpdir)
#'
read_all_html <- function(path) {
  paths <- fs::path_abs(fs::dir_ls(path, glob = "*.html", recurse = TRUE))
  htmls <- lapply(paths, xml2::read_html)
  rel <- fs::path_rel(paths, start = path)
  splits <- vapply(
    fs::path_dir(rel), \(split) {
      if(!endsWith(split, 'instructor')) {
        if(split == ".") split <- "learner"
        split <- paste(split, "learner", sep = "/")
      }
      split
    },
    character(1)
  )
  htmls <- split(htmls, splits)
  slugs <- split(get_slug(rel), splits)
  for (version_id in names(htmls)) {
    names(htmls[[version_id]]) <- slugs[[version_id]]
  }
  c(htmls, list(paths = paths))
}

#' Provision an aggregate page in a lesson
#'
#' A function that will provision page using a reusable template provisioned
#' for aggregate pages used in this lesson to avoid the unnecessary
#' re-rendering of already rendered content.
#'
#' @param pkg an object created via [pkgdown::as_pkgdown()] of a lesson.
#' @param title the new page title
#' @param slug the slug for the page (e.g. "aio" will become "aio.html")
#' @param new if `TRUE`, (default), the page will be generated
#'   from a new template. If `FALSE`, the page is assumed to have been pre-built and
#'   should be appended to.
#' @return
#'  - `provision_agg_page()`: a list:
#'    - `$learner`: an `xml_document` templated for the learner page
#'    - `$instructor`: an `xml_document` templated for the instructor page
#'    - `$needs_episodes`: a logical indicating if the page should be completly
#'       rebuilt (currently default to TRUE)
#'  - `provision_extra_template()`: invisibly, a copy of a global list of HTML
#'   template content available in the internal global object
#'   `sandpaper:::.html`:
#'    - `.html$template$extra$learner`: the rendered learner template as a string
#'    - `.html$template$extra$instructor`: the rendered instructor template as a string
#' @details
#'
#' Pkgdown provides a lot of services in its rendering:
#'
#'  - cross-linking
#'  - syntax highlighting
#'  - dynamic templating
#'  - etc.
#'
#' The problem is that all of this effort takes time and it scales with the size
#' of the page being rendered such that in some cases it can take ~2 seconds for
#' the All in One page of a small lesson. Creating aggregate pages after all of
#' the markdown content has been rendered should not involve re-rendering
#' content.
#'
#' Luckily, reading in content with [xml2::read_html()] is very quick and using
#' XPath queries, we can take the parts we need from the rendered content and
#' insert it into a template, which we store as a character in the `.html`
#' global object so it can be passed around to other functions without needing
#' an argument.
#'
#' `provision_agg_page()` makes a copy of this cache, replaces the FIXME
#'  values with the appropriate elements, and returns an object created from
#'  [xml2::read_html()] for each of the instructor and learner veiws.
#'
#' @keywords internal
#' @rdname provision
#' @examples
#' if (FALSE) { # only run if you have provisioned a pkgdown site
#'   lsn_site <- "/path/to/lesson/site"
#'   pkg <- pkgdown::as_pkgdown(lsn_site)
#'
#'   # create an AIO page
#'   provision_agg_page(pkg, title = "All In One", slug = "aio", quiet = FALSE)
#' }
provision_agg_page <- function(pkg, title = "Key Points", slug = "key-points", new = FALSE) {
  flavor_config <- pkg$meta$template$params$flavors
  flavor_ids <- if (is.null(flavor_config)) "" else {
    which_to_build <- vapply(flavor_config, \(flavor) {
      # default to building if no render property
      if(is.null(flavor[["render"]])) TRUE else flavor[["render"]]
    }, logical(1))
    names(flavor_config)[which_to_build]
  }

  if (new) {
    if (is.null(.html$get()$template$extra)) {
      provision_extra_template(pkg)
    }
    templates <- lapply(
      self_name(flavor_ids), \(flavor_id) lapply(
        self_name(c("learner", "instructor")), \(user_type) {
          template <- .html$get()$template$extra[[flavor_id]][[user_type]]
          template <- gsub("--FIXME TITLE", title, template)
          template <- gsub("--FIXME", slug, template)
          xml2::read_html(template)
        }
      )
    )
  } else {
    templates <- lapply(
      self_name(flavor_ids), \(flavor_id) lapply(
        self_name(c("learner", "instructor")), \(user_type) {
          uri <- as_html(
            slug,
            user_type == "instructor",
            flavor_id
          )
          template <- fs::path(pkg$dst_path, uri)
          xml2::read_html(template)
        }
      )
    )
  }
  templates[["needs_episodes"]] <- new

  templates
}

#' @keywords internal
#' @rdname provision
provision_extra_template <- function(pkg, quiet = TRUE) {
  page_globals <- setup_page_globals()
  needs_episodes <- TRUE
  html <- xml2::read_html("<section id='--FIXME'></section>")
  page <- "--FIXME.html"
  
  flavor_config <- page_globals$meta$get()$flavors
  built_flavor_ids <- if (is.null(flavor_config)) "" else {
    which_to_build <- vapply(flavor_config, \(flavor) {
      # default to building if no render property
      if(is.null(flavor[["render"]])) TRUE else flavor[["render"]]
    }, logical(1))
    names(flavor_config)[which_to_build]
  }

  page_paths <- vapply(built_flavor_ids, \(flavor_id) c(
    fs::path(pkg$dst_path, flavor_id, page),
    fs::path(pkg$dst_path, flavor_id, "instructor", page)
  ), character(2))

  date <- Sys.Date()
  this_dat <- list(
    this_page = page,
    body = html,
    pagetitle = "--FIXME TITLE",
    updated = date
  )
  
  page_globals$instructor$update(this_dat)
  page_globals$learner$update(this_dat)
  page_globals$metadata$update(c(this_dat, list(date = list(modified = date))))

  build_html(
    template = "extra", pkg = pkg, nodes = html,
    global_data = page_globals, path_md = page, quiet = quiet
  )
  on.exit({
    fs::file_delete(page_paths)
  })
  vapply(built_flavor_ids, \(flavor_id) {
    .html$set(c(
      "template",
      "extra",
      # unflavored will have an extra empty string here
      # that's ok. it will get picked elsewhere
      flavor_id,
      "learner"
    ), xml2::read_html(fs::path(pkg$dst_path, flavor_id, page)))
    .html$set(c(
      "template",
      "extra",
      flavor_id,
      "instructor"
    ), xml2::read_html(fs::path(pkg$dst_path, flavor_id, "instructor", page)))
    logical()
  }, logical())
}

section_fun <- function(slug) {
  get(paste0("make_", sub("-", "", slug), "_section"), asNamespace("sandpaper"))
}

#' Build a page for aggregating common elements
#'
#' @inheritParams provision_agg_page
#' @param pages output from the function [read_all_html()]: a nested list of
#'   `xml_document` objects representing episodes in the lesson
#' @param aggregate a selector for the lesson content you want to aggregate.
#'   The default is "section", which will aggregate all sections, but nothing
#'   outside of the sections. To grab everything in the page, use "*"
#' @param append a selector for the section of the page where the aggregate data
#'   should be placed. This defaults to "self::node()", which indicates that the
#'   entire page should be appended.
#' @param prefix flag to add a prefix for the aggregated sections. Defaults to
#'   `FALSE`.
#' @param quiet if `TRUE`, no messages will be emitted. If FALSE, pkgdown will
#'   report creation of the temporary file.
#' @return NULL, invisibly. This is called for its side-effect
#'
#' @details
#'
#' Building an aggregate page is very much akin to copy/paste---you take the
#' same elements across several pages and paste them into one large page. We can
#' do this programmatically by using XPath to extract nodes from pages and add
#' them into the document.
#'
#' To customise the page, we need a few things:
#'
#' 1. a title
#' 2. a slug
#' 3. a method to prepare and insert the extracted content (e.g. [make_aio_section()])
#' @note
#' This function assumes that you have already built all the episodes of your
#' lesson.
#'
#' @keywords internal
#' @rdname build_agg
#' @examples
#' if (FALSE) {
#'   # build_aio() assumes that your lesson has been built and takes in a
#'   # pkgdown object, which can be created from the `site/` folder in your
#'   # lesson.
#'   lsn <- "/path/to/my/lesson"
#'   pkg <- pkgdown::as_pkgdown(fs::path(lsn, "site"))
#'
#'   htmls <- read_all_html(pkg$dst_path)
#'   build_aio(pkg, htmls, quiet = FALSE)
#'   build_keypoints(pkg, htmls, quiet = FALSE)
#' }
build_agg_page <- function(pkg, pages, title = NULL, slug = NULL, aggregate = "section", append = "self::node()", prefix = FALSE, quiet = FALSE) {
  path <- get_source_path() %||% root_path(pkg$src_path)
  out_path <- pkg$dst_path
  this_lesson(path)
  pages_html <- pages
  pages_html[['paths']] <- NULL

  new_content <- append == "self::node()" || append == "self::*"
  agg <- provision_agg_page(pkg, title = title, slug = slug, new = new_content)
  agg$needs_episodes <- NULL
  flavor_ids <- names(agg)
  if (new_content) {
    lapply(agg, \(x) lapply(x, \(template) {
      remove_fix_node(template, slug)
    }))
  }

  # Get sectioning function definied in the `build_` file. For example,
  # build_instructor_notes.R contains a function called
  # `make_instructornotes_section`
  make_section <- section_fun(slug)

  parents <- lapply(agg, \(x) lapply(x, \(template) {
    get_content(template, content = append)
  }))
  needs_content <- !new_content && all(vapply(parents, \(template_pair) length(
    template_pair[["instructor"]]
  ), numeric(1)) == 0)
  if (needs_content) {
    # When the content requested does not exist, we append a new section with
    # the id of aggregate-{slug}
    sid <- paste0("aggregate-", slug)
    contents <- lapply(names(agg), \(flavor_id) lapply(
      c('learner', 'instructor'), 
      \(user_type) {
        content <- get_content(agg[[flavor_id]][[user_type]], content = "self::node()")
        xml2::xml_add_child(content, "section", id = sid)
        parents[[flavor_id]][[user_type]] <- xml2::xml_child(content, xml2::xml_length(content))
        content
      }
    ))
  }
  # clean up any content that currently exists
  lapply(parents, \(x) lapply(x, \(parent) {
    xml2::xml_remove(xml2::xml_children(parent))
  }))

  the_episodes <- .resources$get()[["episodes"]]
  the_slugs <- get_slug(the_episodes)
  the_slugs <- if (prefix) paste0(slug, "-", the_slugs) else the_slugs
  for (episode in seq(the_episodes)) {
    ep_learn <- ep_instruct <- the_episodes[episode]
    ename <- the_slugs[episode]
    if (!is.null(pages_html)) {
      name <- if (prefix) sub(paste0("^", slug, "-"), "", ename) else ename
      ep <- lapply(pages_html, \(x) x[[name]])
    }
    ep_title <- as.character(xml2::xml_contents(get_content(ep[[1]], ".//h1")))
    names(ename) <- paste(ep_title, collapse = "")
    for (i in seq_along(ep[[1]])){
      fid_i <- strsplit(names(ep)[i], "/", fixed = TRUE)[[1]]
      if(length(fid_i) == 1) {
        flavor_id <- ""
        user_type <- fid_i
      } else {
        flavor_id <- fid_i[1]
        user_type <- fid_i[2]
      }
      ep[[i]] <- get_content(
        ep[[i]], content = aggregate, pkg = pkg,
        instructor = user_type == "instructor",
        flavor_id = if(flavor_id == "") NULL else flavor_id
      )
      make_section(ename, ep[[i]], parents[[flavor_id]][[user_type]], instructor = user_type == "instructor")
    }
  }
  
  report <- "Writing '{.file {rel_out_path}}'"
  outs <- lapply(flavor_ids, \(flavor_id) lapply(
    c("learner", "instructor"),
    \(user_type) {
      rel_out_path <- as_html(
        slug,
        instructor = user_type == "instructor",
        flavor_id = flavor_id
      )
      full_out_path <- fs::path(out_path, rel_out_path)
      if (!quiet) cli::cli_text(report)
      writeLines(as.character(agg[[flavor_id]][[user_type]]), full_out_path)
    }
  ))
}

#' Get sections from an episode's HTML page
#'
#' @param episode an object of class `xml_document`, a path to a markdown or
#'   html file of an episode.
#' @inheritParams build_aio
#' @param content an XPath fragment. defaults to "*"
#' @param label if `TRUE`, elements will be named by their ids. This is best
#'   used when content = "section".
#' @param instructor if `TRUE`, the instructor version of the episode is read,
#'   defaults to `FALSE`. This has no effect if the episode is an `xml_document`.
#'
#' @details
#' The contents of the lesson are contained in the following templating cascade:
#'
#' ```html
#' <body>
#'   <div class='container'>
#'     <div class='row'>
#'       <div class='[...] primary-content'>
#'         <main>
#'           <div class='[...] lesson-content'>
#'             CONTENT HERE
#' ```
#'
#' This function will extract the content from the episode without the templating.
#'
#' @keywords internal
#' @examples
#' if (FALSE) {
#'   lsn <- "/path/to/lesson"
#'   pkg <- pkgdown::as_pkgdown(fs::path(lsn, "site"))
#'
#'   # for AiO pages, this will return only the top-level sections:
#'   get_content("aio", content = "section", label = TRUE, pkg = pkg)
#'
#'   # for episode pages, this will return everything that's not template
#'   get_content("01-introduction", pkg = pkg)
#'
#'   # for things that are within lessons but we don't know their exact location,
#'   # we can prefix a `/` to double up the slash, which will produce
#' }
get_content <- function(
    episode, content = "*", label = FALSE, pkg = NULL,
    instructor = FALSE, flavor_id = NULL) {
  if (!inherits(episode, "xml_document")) {
    path <- fs::path(pkg$dst_path, as_html(episode, instructor = instructor, flavor_id = flavor_id))
    episode <- xml2::read_html(path)
  }
  XPath <- ".//main/div[contains(@class, 'lesson-content')]/{content}"
  res <- xml2::xml_find_all(episode, glue::glue(XPath))
  if (label) {
    names(res) <- xml2::xml_attr(res, "id")
  }
  res
}

# escape pesky ampersands in output text
escape_ampersand <- function(text) {
  gsub("[&](?![#]?[A-Za-z0-9]+?[;])", "&amp;", text, perl = TRUE)
}

remove_fix_node <- function(html, id = "FIXME") {
  fix_node <- xml2::xml_find_first(html, paste0(".//section[@id='", id, "']"))
  xml2::xml_remove(fix_node)
  return(html)
}

build_sitemap <- function(out, paths = NULL, quiet = TRUE) {
  if (!quiet) cli::cli_rule(cli::style_bold("Creating sitemap.xml"))
  url <- this_metadata$get()$url
  paths <- paths %||% fs::dir_ls(out, glob = "*.html", recurse = TRUE)
  urls <- paste0(url, fs::path_rel(paths, out))
  doc <- urls_to_sitemap(urls)
  sitemap <- fs::path(out, "sitemap.xml")
  xml2::write_xml(doc, file = sitemap)
  invisible()
}

urls_to_sitemap <- function(urls) {
  doc <- xml2::read_xml("<urlset xmlns='http://www.sitemaps.org/schemas/sitemap/0.9'></urlset>")
  for (url in urls) {
    child <- xml2::read_xml(paste0("<url><loc>", url, "</loc></url>"))
    xml2::xml_add_child(doc, child)
  }
  doc
}

self_name <- \(x) structure(x, names = x)

