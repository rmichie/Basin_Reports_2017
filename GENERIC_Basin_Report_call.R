
# Script to generate all the basin reports

setwd("//deqhq1/WQNPS/NPS_Annual_Reports/2017/Basin_Reports_2017/")

output.dir = "//deqhq1/WQNPS/NPS_Annual_Reports/2017/Basin_Reports_2017/"

##owrd_basin <- "Deschutes"
##owrd_basin <- "Goose & Summer Lakes"
##owrd_basin <- "Grande Ronde"
##owrd_basin <- "Hood"
##owrd_basin <- "John Day"
##owrd_basin <- "Klamath"
##owrd_basin <- "Malheur"
##owrd_basin <- "Malheur Lakes"
##owrd_basin <- "Mid Coast"
##owrd_basin <- "North Coast"  # See other script
##owrd_basin <- "Owyhee"
##owrd_basin <- "Powder"
##owrd_basin <- "Rogue"
##owrd_basin <- "Sandy"
##owrd_basin <- "South Coast"
##owrd_basin <- "Umatilla"
##owrd_basin <- "Umpqua"
owrd_basin <- "Willamette"

owrd_basins <- c("Deschutes",
                 "Goose & Summer Lakes",
                 "Grande Ronde",
                 "Hood",
                 "John Day",
                 "Klamath",
                 "Malheur",
                 "Malheur Lakes",
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
                        "Grande Ronde"="c",
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
                        "South Coast"="N",
                        "Umatilla"="O",
                        "Umpqua"="P",
                        "Willamette"="Q")




for (owrd_basin in owrd_basins) {

  print(owrd_basin)

  # Set the correct Appendix letter
  a.letter <- appendix_letter[[owrd_basin]]
  
  report.title.docx <- paste("Appendix ", a.letter)
  report.subtitle.docx <- paste(owrd_basin, " Basin Report")
  
  report.title.html <- paste(owrd_basin, " Basin Report")
  report.subtitle.html <- paste("2017 Oregon Nonpoint Source Annual Report Appendix ", a.letter)
  
  report.name.html <-gsub(" ", "_", paste0(owrd_basin,"_Basin_Report.html"), fixed = TRUE)
  report.name.docx <-gsub(" ", "_", paste0(owrd_basin,"_Basin_Report.docx"), fixed = TRUE)
  
  # WORD
  rmarkdown::render(input="GENERIC_Basin_Report.Rmd",
                    params = list(owrd_basin=owrd_basin,
                                  report_title = report.title.docx,
                                  report_subtitle = report.subtitle.docx,
                                  a.letter=a.letter),
                    output_format = "word_document",
                    output_dir = output.dir,
                    output_file=report.name.docx)
  
  # HTML
  #rmarkdown::render(input="GENERIC_Basin_Report.Rmd",
  #                  params = list(owrd_basin=owrd_basin,
  #                                report_title = report.title.html,
  #                                report_subtitle = report.subtitle.html,
  #                                a.letter=a.letter),
  #                                output_format = "html_document",
  #                  output_dir = output.dir,
  #                  output_file=report.name.html)
  
}

print("done")





