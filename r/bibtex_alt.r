bib2acad2 <-function (bibfile = "", copybib = TRUE, abstract = TRUE, overwrite = FALSE)
{
  msg1 <- "You must specify a .bib file as input for the conversion."
  msg2 <- paste0("Cannot find file '", bibfile, "'. Check path and/or file name.")
  if (bibfile == "") {
    return(message(msg1))
  }
  if (!file.exists(bibfile)) {
    return(message(msg2))
  }
  outfold <- "my-md-folder"
  pubfold <- "my-pdf-folder"
  if (copybib) {
    bibfold <- "my-bib-folder"
  }
  dir.create("my-md-folder", showWarnings = FALSE)
  dir.create("my-pdf-folder", showWarnings = FALSE)
  dir.create("my-bib-folder", showWarnings = FALSE)
  mypubs <- RefManageR::ReadBib(bibfile, check = "warn", .Encoding = "UTF-8")
  mypubs <- as.data.frame(mypubs)
  mypubs$key <- rownames(mypubs)
  mypubs <- dplyr::mutate(mypubs, pubtype = dplyr::case_when(bibtype ==
                                                               "Article" ~ "2", bibtype == "Article in Press" ~ "2",
                                                            bibtype == "InProceedings" ~ "1", bibtype == "Proceedings" ~
                                                               "1", bibtype == "Conference" ~ "1", bibtype == "Conference Paper" ~
                                                               "1", bibtype == "MastersThesis" ~ "3", bibtype ==
                                                               "PhdThesis" ~ "3", bibtype == "Manual" ~ "4", bibtype ==
                                                               "TechReport" ~ "4", bibtype == "Book" ~ "5", bibtype ==
                                                               "InCollection" ~ "6", bibtype == "InBook" ~ "6",
                                                             bibtype == "Book Chapter" ~ "6", bibtype == "Misc" ~ "0", TRUE ~ "0"))
  create_md <- function(x) {
    if (!is.na(x[["date"]])) {
      if(nchar(x[["date"]]) == 4){
        x[["date"]] <- paste0(substr(x[["date"]],1,4), "-01-01")
      }
      if(nchar(x[["date"]]) == 7){
        x[["date"]] <- paste0(substr(x[["date"]],1,7), "-01")
      }
      if(nchar(x[["date"]]) == 10){
        x[["date"]] <- x[["date"]]
      }
    }
    else {
      x[["date"]] <- "2999-01-01"
    }
    filename_md <- paste0(x[["key"]], ".md")
    if (!file.exists(file.path(outfold, filename_md)) | overwrite) {
      fileConn <- file.path(outfold, filename_md)
      write("+++", fileConn)
      write(paste0("title = \"", cleanStr(x[["title"]]),
                   "\""), fileConn, append = T)
      write(paste0("date = \"", x[["date"]], "\""), fileConn,
            append = T)
      write(paste0("publication_types = [\"", x[["pubtype"]],
                   "\"]"), fileConn, append = T)
      if (!is.na(x[["author"]])) {
        authors <- stringr::str_replace_all(stringr::str_squish(x["author"]),
                                            " and ", "\", \"")
        #authors <- stringr::str_remove_all(authors, "{")
        #authors <- stringr::str_remove_all(authors, "}")
        authors <- stringr::str_replace(authors, "Thomas P. C. Dorlo",
                                        "**Thomas P. C. Dorlo**")
        authors <- stringr::str_replace(authors, "T. P. C. Dorlo",
                                        "**T. P. C. Dorlo**")
        authors <- stringi::stri_trans_general(authors,
                                               "latin-ascii")
        write(paste0("authors = [\"", cleanStrA(authors), "\"]"),
              fileConn, append = T)
      }
      else {
        editors <- stringr::str_replace_all(stringr::str_squish(x["editor"]),
                                            " and ", "\", \"")
        editors <- stringi::stri_trans_general(editors,
                                               "latin-ascii")
        write(paste0("editors = [\"", editors, "\"]"),
              fileConn, append = T)
      }
      publication <- NULL
      if ("booktitle" %in% names(x) && !is.na(x[["booktitle"]])) {
        publication <- paste0(publication, "In: ", cleanStr(x[["booktitle"]]))
        if ("publisher" %in% names(x) && !is.na(x[["publisher"]])) {
          publication <- paste0(publication, ", ", cleanStr(x[["publisher"]]))
        }
        if ("address" %in% names(x) && !is.na(x[["address"]])) {
          publication <- paste0(publication, ", ", cleanStr(x[["address"]]))
        }
        if ("pages" %in% names(x) && !is.na(x[["pages"]])) {
          publication <- paste0(publication, ", _pp. ",
                                cleanStr(x[["pages"]]), "_")
        }
      }
      if ("journaltitle" %in% names(x) && !is.na(x[["journaltitle"]])) {
        publication <- paste0(publication, "_", cleanStr(x[["journaltitle"]]), "_")
        #if ("number" %in% names(x) && !is.na(x[["number"]])) {
        #  publication <- paste0(publication, " ", cleanStr(x[["number"]]))
        #}
        #if ("volume" %in% names(x) && !is.na(x[["volume"]])) {
        #  publication <- paste0(publication, " (", cleanStr(x[["volume"]]),
        #                        ") ")
        #}
        #if ("pages" %in% names(x) && !is.na(x[["pages"]])) {
        #  publication <- paste0(publication, ": ",
        #                        cleanStr(x[["pages"]]), "_")
        #}
        #if ("doi" %in% names(x) && !is.na(x[["doi"]])) {
        #  publication <- paste0(publication, ", ", paste0("https://doi.org/",
        #                                                  cleanStr(x[["doi"]])))
        #}
        #if ("url" %in% names(x) && !is.na(x[["url"]])) {
        #  publication <- paste0(publication, ", ", cleanStr(x[["url"]]))
        #}
      }
      write(paste0("publication = \"", publication, "\""),
            fileConn, append = T)
      if ((abstract) && "abstract" %in% names(x) && !is.na(x[["abstract"]])) {
        write(paste0("abstract = \"", cleanStr(x[["abstract"]]),
                     "\""), fileConn, append = T)
      }
      else {
        write("abstract = \"\"", fileConn, append = T)
      }

      if ("doi" %in% names(x) && !is.na(x[["doi"]])) {
        write(paste0("doi = \"", cleanStr(x[["doi"]]),
                     "\""), fileConn, append = T)
      }
      else {
        write("doi = \"\"", fileConn, append = T)
      }

      if ("pmid" %in% names(x) && !is.na(x[["pmid"]])) {
        write(paste0("links = [{name = \"PubMed\", url = \"https://www.ncbi.nlm.nih.gov/pubmed/", cleanStr(x[["pmid"]]),
                     "\"}]"), fileConn, append = T)
      }
      if ("url" %in% names(x) && !is.na(x[["url"]])) {
        write(paste0("links = [{name = \"Web\", url = \"",x[["url"]],
                     "\"}]"), fileConn, append = T)
      }


      # url_custom = [{name = "Custom Link", url = "http://example.org"}]

      filename_pdf <- (gsub(".md", ".pdf", filename_md))

      if (file.exists(file.path("static/pdf", filename_pdf))) {
        write(paste0("url_pdf = \"pdf/", filename_pdf,"\""), fileConn, append = T)
      }

      else if ("file" %in% names(x) && !is.na(x[["file"]])) {
        write(paste0("url_pdf = \"pdf/", filename_pdf,"\""), fileConn, append = T)
      }

      else {
        write("url_pdf = \"\"", fileConn, append = T)
      }

      write(paste0("abstract_short = \"", "\""), fileConn,
            append = T)
      write("image_preview = \"\"", fileConn, append = T)
      write("selected = false", fileConn, append = T)
      write("projects = []", fileConn, append = T)
      write("tags = []", fileConn, append = T)
      write("url_preprint = \"\"", fileConn, append = T)
      write("url_code = \"\"", fileConn, append = T)
      write("url_dataset = \"\"", fileConn, append = T)
      write("url_project = \"\"", fileConn, append = T)
      write("url_slides = \"\"", fileConn, append = T)
      write("url_video = \"\"", fileConn, append = T)
      write("url_poster = \"\"", fileConn, append = T)
      write("url_source = \"\"", fileConn, append = T)
      write("math = true", fileConn, append = T)
      write("highlight = true", fileConn, append = T)
      write("[header]", fileConn, append = T)
      write("image = \"\"", fileConn, append = T)
      write("caption = \"\"", fileConn, append = T)
      write("+++", fileConn, append = T)
    }
    if (copybib) {
      filename_bib <- (gsub(".md", ".bib", filename_md))
      y <- as.list(x)
      y["pubtype"] <- NULL
      y <- RefManageR::as.BibEntry(y)
      if (!file.exists(file.path(bibfold, filename_bib)) |
          overwrite) {
        RefManageR::WriteBib(y, file = file.path(bibfold,
                                                 filename_bib), verbose = FALSE)
      }
    }
    if ("file" %in% names(x) && !is.na(x[["file"]])) {
      filename_pdf <- (gsub(".md", ".pdf", filename_md))
      pdfloc <- gsub("Full Text:", "", x[["file"]])
      pdfloc <- gsub("Accepted Version:", "", pdfloc, fixed=TRUE)
      pdfloc <- gsub("C\\:", "C:", pdfloc, fixed=TRUE)
      pdfloc <- gsub(":application/pdf","", pdfloc, fixed=TRUE)
      file.rename(from = pdfloc, to = file.path(pubfold, filename_pdf))
      }

  }
  pb <- pbapply::startpb(min = 0, max = nrow(mypubs))
  pbapply::pbapply(mypubs, FUN = function(x) create_md(x),
                   MARGIN = 1)
  pbapply::closepb(pb)
}

cleanStr <- function(str) {
  # if special character has in front a "\": replace it with "\\\\"
  str <- gsub('\\', '\\\\', str, fixed = TRUE)
  # delete all "{" and "}" in old bibtex files
  str <- gsub("[{}]", '', str)
  # replace all inline quotes '"' with "four '\\\\"'
  str <- gsub('"', '\\\\"', str)
  # delete extra lines, tabs and spaces
  # (especially important with field 'abstract')
  # and return the cleaned string
  return(stringr::str_squish(str))
}

cleanStrA <- function(str) {
  # if special character has in front a "\": replace it with "\\\\"
  str <- gsub('\\', '\\\\', str, fixed = TRUE)
  # delete all "{" and "}" in old bibtex files
  str <- gsub("[{}]", '', str)
  # replace all inline quotes '"' with "four '\\\\"'
  #str <- gsub('"', '\\\\"', str)
  # delete extra lines, tabs and spaces
  # (especially important with field 'abstract')
  # and return the cleaned string
  return(stringr::str_squish(str))
}


opendir <- function(dir = getwd()){
  if (.Platform['OS.type'] == "windows"){
    shell.exec(dir)
  } else {
    system(paste(Sys.getenv("R_BROWSER"), dir))
  }
}

#bib2acad2(bibfile="C:/Users/thoma/Dropbox/Site/academic-kickstart-master/content/publication/bibtex/Dorlo10.bib", overwrite=T)

#f <- "Full Text:C\:\\Users\\thoma\\Zotero\\storage\\8PE97TAN\\de Souza and Dorlo - 2018 - Safe mass drug administration for neglected tropic.pdf:application\pdf"
