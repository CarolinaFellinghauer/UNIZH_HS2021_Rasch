library(mirtCAT)

## Call the SRG-Data
urlfile = "https://raw.githubusercontent.com/CarolinaFellinghauer/UNIZH_HS2020_Rasch/master/Data/SRG_Data_Course_UNIZH.csv?token=AB5GB47UIUWV7F5NMGA33T27K5IQ2"

srg.data=read.csv(url(urlfile))

dim(srg.data)
colnames(srg.data)


#Items (minus SRG-15 which was misfitting)
srg.items=c("SRG1", "SRG2",  "SRG3",  "SRG4",  "SRG5",  "SRG6",  "SRG7",  "SRG8",  "SRG9", "SRG10", "SRG11", "SRG12", "SRG13", "SRG14") # minus "SRG15"

#CAT parameter
start = "random"
next_item = "MI"
score_function = "WLE"
stop = list(min_SEM = 0.5)


# Obtaining a calibrated item bank
mirt.srg = mirt(srg.data[,srg.items], 1, itemtype = "Rasch")

abil.srg = fscores(mirt.srg, method = score_function)
min.abil.srg = min(abil.srg, na.rm = TRUE)
max.abil.srg = max(abil.srg, na.rm = TRUE)


####preparing an assessment interface:

#create the data frame of items and responses
options = matrix(c("not at all", "somewhat", "a great deal"), 
                 ncol = 3, nrow = 14, byrow = TRUE)

questions = c("SRG1: I learned to be nicer to others.",
              "SRG2: I feel freer to make my own decisions.",
              "SRG3: I learned that I have something of value to teach others about life.",
              "SRG4: I learned to be myself and not try to be what others want me to be.",
              "SRG5: I learned to work through models and not just give up.",
              "SRG6: I learned to find more meaning in life.",
              "SRG7: I learned to how to reach out and help others.",
              "SRG8: I learned to be a more confident person.",
              "SRG9: I learned to listen more carefully when others talk to me.",
              "SRG10: I learned to be open to new information and ideas.",
              "SRG11: I learned to communicate more honestly with others.",
              "SRG12: I learned that I want to have some impact on the world.",
              "SRG13: I learned that it's okay to ask others for help.",
              "SRG14: I learned to stand up for my personal rights.")

df <- data.frame(Question = questions, Option = options, Type = "radio")


# change some aesthetics of the interface
title <- "Stress Related Growth"
authors <- "Park - 1996"
firstpage <- list(h2("SRG-CAT"), h5("Please answer each item to the best of your ability. ",        
                                    "The results of this test will remain completely anonymous ", 
                                    "and are only used for research purposes."))
                                         
lastpage <- function(person){
  list(h2("Thank you for completing the test."), 
       h4(sprintf('Your final SRG is:  %.3f', person$thetas[1])),
       h4(sprintf("Percentage of SRG is: %.3f", (person$thetas[1] - min.abil.srg)/(max.abil.srg-min.abil.srg) *100  )))  #,
                   #person$thetas_SE_history[nrow(person$thetas_SE_history), 1])))
}
demographics <- list(
  selectInput(inputId = "gender", label = "Please select your gender.",
                               choices = c("Male", "Female", "Other"), selected = ""),
  selectInput(inputId = "age", label = "Please select your age group.", 
                               choices = c("16-25", "26-35", "36-45", "46-55", "56-65", "> 65 years"), selected = ""))
                               
shinyGUI_list <- list(title = title, authors = authors, demographics = demographics,
                      demographics_inputIDs = c("gender", "age"), firstpage = firstpage, lastpage = lastpage)


## running the assessment

result =  mirtCAT(df = df, mo = mirt.srg, 
         method = score_function, start_item = start, 
         criteria = next_item,  design = stop, shinyGUI = shinyGUI_list)

#saving the output
Output = as.matrix(as.character(c(result$demographics$age, result$demographics$gender, result$raw_responses, result$scored_responses, result$thetas, result$SE_thetas), nrow = 1))
Output = as.data.frame(t(Output))
colnames(Output) = c("Age", "Gender", paste("SRG_", 1:14, "_lbl", sep = ""), paste("SRG_", 1:14, sep = ""), "Ability", "SE_Ability")
 
#first run
#create an output table - can be done manually also.

write.table(
  Output,
  file = "C:/Users/carol/OneDrive/Desktop/Kurs_HS_2020/Inhalt/13_CAT/SRG_Cat.txt",
  sep = ",",
  col.names = TRUE,
  row.names = TRUE)

#next runs

# write.table(
#   Output,
#   file = "C:/Users/carol/OneDrive/Desktop/Kurs_HS_2020/Inhalt/13_CAT/SRG_Cat.txt",
#   sep =",",
#   col.names = FALSE,
#   row.names = TRUE,
#   append = TRUE)

#results can be saved in an excel file on the test computer, a server, into the cloud...

