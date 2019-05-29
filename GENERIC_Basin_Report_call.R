
# Script to generate all the basin reports

setwd("C:/Basin_Reports_2018/")

output.dir = "C:/Basin_Reports_2018/"

# owrd_basin <- "Deschutes"
# owrd_basin <- "Goose & Summer Lakes"
# owrd_basin <- "Grande Ronde"
# owrd_basin <- "Hood"
# owrd_basin <- "John Day"
# owrd_basin <- "Klamath"
# owrd_basin <- "Malheur"
# owrd_basin <- "Malheur Lakes"
# owrd_basin <- "Mid Coast"
# owrd_basin <- "North Coast"  # See other script
# owrd_basin <- "Owyhee"
# owrd_basin <- "Powder"
# owrd_basin <- "Rogue"
# owrd_basin <- "Sandy"
# owrd_basin <- "South Coast"
# owrd_basin <- "Umatilla"
# owrd_basin <- "Umpqua"
# owrd_basin <- "Willamette"

owrd_basins <- c("Deschutes",
                 "Goose & Summer Lakes",
                 "Grande Ronde",
                 "Hood",
                 "John Day",
                 "Klamath",
                 "Malheur",
                 "Malheur Lakes",
                 "Mid Coast",
                 "North Coast",
                 "Owyhee",
                 "Powder",
                 "Rogue",
                 "Sandy",
                 "South Coast",
                 "Umatilla",
                 "Umpqua",
                 "Willamette")




# List of the Appendix letter prefix for each basin
appendix_letter <- list("Deschutes"="A",
                        "Goose & Summer Lakes"="B",
                        "Grande Ronde"="C",
                        "Hood"="D",
                        "John Day"="E",
                        "Klamath"="F",
                        "Malheur"="G",
                        "Malheur Lakes"="H",
                        "Mid Coast"="I",
                        "North Coast"="J",
                        "Owyhee"="K",
                        "Powder"="L",
                        "Rogue"="M",
                        "Sandy"="N",
                        "South Coast"="O",
                        "Umatilla"="P",
                        "Umpqua"="Q",
                        "Willamette"="R")




for (owrd_basin in owrd_basins) {

  print(owrd_basin)

  # Set the correct Appendix letter
  a.letter <- appendix_letter[[owrd_basin]]
  
  report.title.docx <- paste0("Appendix ", a.letter)
  report.subtitle.docx <- paste0(owrd_basin, " Basin Report")
  
  report.title.html <- paste0(owrd_basin, " Basin Report")
  report.subtitle.html <- paste0("2018 Oregon Nonpoint Source Annual Report Appendix ", a.letter)
  
  report.name.html <-gsub(" ", "_", paste0(report.title.docx," ",owrd_basin,"_Basin_Report.html"), fixed = TRUE)
  report.name.docx <-gsub(" ", "_", paste0(report.title.docx," ",owrd_basin,"_Basin_Report.docx"), fixed = TRUE)
  
  # WORD
  rmarkdown::render(input="C:/Basin_Reports_2018/GENERIC_Basin_Report.Rmd",
                    params = list(owrd_basin=owrd_basin,
                                  report_title = report.title.docx,
                                  report_subtitle = report.subtitle.docx,
                                  a.letter=a.letter,
                                  html_output=FALSE),
                    output_format = "word_document",
                    output_dir = output.dir,
                    output_file=report.name.docx)
  
  # HTML
  #rmarkdown::render(input="GENERIC_Basin_Report.Rmd",
  #                  params = list(owrd_basin=owrd_basin,
  #                                report_title = report.title.html,
  #                                report_subtitle = report.subtitle.html,
  #                                a.letter=a.letter,
  #                                html_output=TRUE),
  #                                output_format = "html_document",
  #                  output_dir = output.dir,
  #                  output_file=report.name.html)
  
}

print("done")





