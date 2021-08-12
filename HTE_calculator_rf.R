# This code implements an automated GUI risk score calculator , using the Shiny package in R.
# install the shiny app before using

library(DT)
library(shiny)

# Load saved models 
setwd("C:/Users/cryst/Documents/Stanford Postdoc/NHLBI R01 Aim 2/Analyses Stanford Team/Analysis Results/Tuned Results")
fit0_cvd <- readRDS("sprint_fit0_cvd.rds")
fit1_cvd <- readRDS("sprint_fit1_cvd.rds")
fit0_sae <- readRDS("sprint_fit0_sae.rds")
fit1_sae <- readRDS("sprint_fit1_sae.rds")

# specify.decimal <- function(t, nsmall=3, zero=T) {
#   if (round(t, nsmall) == 0 & !zero) {
#     return(signif(t, 1))
#   }
#   return(format(round(t, nsmall), nsmall=nsmall))
# }


ui = navbarPage("ASCVD",
                
                tabPanel("Treatment Effect Calculator",
                         
                         fluidPage(
                           
                           titlePanel("Treatment Effect Equation of Intensive Blood Pressure Therapy at 3.26 year"),
                           
                           fluidRow(
                             column(width=4,
                                    numericInput("AGE", label = "Age (years)", value = 75),
                                    selectInput("FEMALE", label = "Sex", choices = list(Male=1, Female=0)),
                                    radioButtons("RACE_BLACK", label = "Black?", choices = list(No=0, Yes=1), inline = TRUE),
                                    radioButtons("Hispanic", label = "Hispanic?", choices = list(No=0, Yes=1), inline = TRUE),
                                    numericInput("BMI", label = "Body mass index (kg/m^2)", value = 30),
                                    radioButtons("currentsmoker", label = "Currently smoking tobacco?", choices = list(No=0, Yes=1), inline = TRUE), 
                                    radioButtons("formersmoker", label = "Formerly smoked tobacco?", choices = list(No=0, Yes=1), inline = TRUE),
                                    radioButtons("neversmoker", label = "Have never smoked tobacco?", choices = list(No=0, Yes=1), inline = TRUE),
                                    selectInput("insurance", label = "Insurance coverage", choices = list(Other=0, Private=1)),
                                    radioButtons("LIVEWITHOTHERS", label = "Live with other adults?", choices = list(No=0, Yes=1), inline = TRUE),
                                    numericInput("SBP", label = "Systolic blood pressure (mm Hg)", value = 140),
                                    numericInput("DBP", label = "Diastolic blood pressure (mm Hg)", value = 78)
                             ),
                             column(width=4,
                                    numericInput("SEATHEART", label = "Seated heart rate (beats/min)", value = 66),
                                    numericInput("EGFR", label = "Estimated glomerular filtration rate (mL/min/1.73m^2)", value = 72),
                                    numericInput("SCREAT", label = "Serum creatinine (mg/dL)", value = 1.1),
                                    numericInput("CHR", label = "Total cholesterol (mg/dL)", value = 190),
                                    numericInput("HDL", label = "HDL cholesterol (mg/dL)", value = 53),
                                    numericInput("GLUR", label = "Glucose (mg/dL)", value = 99),
                                    numericInput("TRR", label = "Triglycerides (mg/dL)", value = 126),
                                    numericInput("RESULT_K", label = "Serum potassium (mmol/L)", value = 4.2),
                                    numericInput("UMALCR", label = "Urine Albumin/Creatinine ratio (mg/g Cr)", value = 39),
                                    radioButtons("SUB_CKD", label = "History CKD (eGFR<60)?", choices = list(No=0, Yes=1), inline = TRUE),
                                    radioButtons("SUB_CLINICALCVD", label = "History of clinical CVD?", choices = list(No=0, Yes=1), inline = TRUE),
                                    radioButtons("CORONARYREVAS", label = "Had coronary revascularization?", choices = list(No=0, Yes=1), inline = TRUE)
                             ),
                             column(width=4,
                                    
                                    radioButtons("ULCER", label = "Had gastric or peptic ulcer?", choices = list(No=0, Yes=1), inline = TRUE),
                                    radioButtons("DEPRESS", label = "Had depression?", choices = list(No=0, Yes=1), inline = TRUE),
                                    radioButtons("ALCOHOL", label = "Had alcohol abuse?", choices = list(No=0, Yes=1), inline = TRUE),
                                    
                                    radioButtons("STATIN", label = "On any statin?", choices = list(No=0, Yes=1), inline = TRUE),
                                    selectInput("N_AGENTS", label = "Number of anti-hypertensive medications prescribed?", choices = 0:10),
                                    radioButtons("ASPIRIN", label = "Daily Aspirin use?", choices = list(No=0, Yes=1), inline = TRUE),
                                    radioButtons("ACE_ARB", label = "Used any angiotensin converting enzyme inhibitors(ACEI) or angiotensin-receptor blockers (ARB)?", choices = list(No=0, Yes=1), inline = TRUE),
                                    radioButtons("CCB", label = "Used any calcium channel blockers?", choices = list(No=0, Yes=1), inline = TRUE),
                                    radioButtons("ThiazideDiuretic", label = "Used any Thiazide diuretics?", choices = list(No=0, Yes=1), inline = TRUE),
                                    radioButtons("LoopDiuretic", label = "Used any Loop diuretics?", choices = list(No=0, Yes=1), inline = TRUE),
                                    radioButtons("BetaBlocker", label = "Used any beta-blockers?", choices = list(No=0, Yes=1), inline = TRUE),
                                    radioButtons("AlphaOneBlocker", label = "Used any Alpha-blockers?", choices = list(No=0, Yes=1), inline = TRUE),
                                    radioButtons("OtherAntiHyperMed", label = "Used any other antihypertensive medications?", choices = list(No=0, Yes=1), inline = TRUE)
                                    #radioButtons("dm", label = "Diabetes mellitus?", choices = list(No=0, Yes=1), inline = TRUE)
                             )
                           ),
                           
                           hr(),
                           
                           fluidRow(
                             column(width=8,
                                    h4("Estimated 3.26-yr Treatment Effect of Intensive Blood Pressure Therapy on the CVD Outcome:", align = "left")
                             ),
                             column(width=4,
                                    h4(textOutput("HTE_CVD"), align = "left")
                             )
                           ),
                           
                           hr(),
                           
                           fluidRow(
                             column(width=8,
                                    h4("Estimated 3.26-yr Treatment Effect of Intensive Blood Pressure Therapy on the SAE Outcome:", align = "left")
                             ),
                             column(width=4,
                                    h4(textOutput("HTE_SAE"), align = "left")
                             )
                           ),
                           
                           hr(),
                           "Note: This calculator is intended for informational purposes only, and has not been prospectively 
                           evaluated for impact on clinical practice or patient outcomes. Calculations must be re-checked and 
                           should not be used alone to guide patient care, nor should they substitute for clinical judgment.
                           Contact: Yizhe Xu, yizhex@stanford.edu
                           
                           "
                           
                         )
                         
                ),
                
                # tabPanel("Summary Statistics",
                #          
                #          h4("Risk model was derived from ARIC, JHS, MESA, CHS, CARDIA, and the Framingham Offspring Study. Summary statistics for the pooled development and validation cohorts are presented here:"),
                #          
                #          br(),
                #          
                #          fluidRow(
                #            column(width=12,
                #                   align="center",
                #                   tableOutput('summary')
                #            )
                #          )
                #          
                # ),
                
                
                tabPanel("Disclaimers",
                         
                         h5("This website contains clinical tools and data intended for use by healthcare professionals. These tools do not give professional advice; physicians and other healthcare professionals who use these tools or data should exercise their own clinical judgment as to the information they provide. Consumers who use the tools or data do so at their own risk. Individuals with any type of medical condition are specifically cautioned to seek professional medical advice before beginning any sort of health treatment. For medical concerns, including decisions about medications and other treatments, users should always consult their physician or other qualified healthcare professional.
                            
                            Our content developers have carefully tried to create its content to conform to the standards of professional practice that prevailed at the time of development. However, standards and practices in medicine change as new data become available and the individual medical professional should consult a variety of sources.
                            
                            The contents of the Site, such as text, graphics and images are for informational purposes only. We do not recommend or endorse any specific tests, physicians, products, procedures, opinions, or other information that may be mentioned on the Site.
                            
                            While information on this site has been obtained from sources believed to be reliable, neither we nor our content providers warrant the accuracy of the information contained on this site.
                            
                            We do not give medical advice, nor do we provide medical or diagnostic services. Medical information changes rapidly. Neither we nor our content providers guarantee that the content covers all possible uses, directions, precautions, drug interactions, or adverse effects that may be associated with any therapeutic treatments.
                            
                            Your reliance upon information and content obtained by you at or through this site is solely at your own risk. Neither we nor our content providers assume any liability or responsibility for damage or injury (including death) to you, other persons or property arising from any use of any product, information, idea or instruction contained in the content or services provided to you.
                            
                            We cannot and will not be held legally, financially, or medically responsible for decisions made using these calculators, equations, and algorithms, and this Site is for the use of medical professionals only."),
                         
                         br(),
                         
                         h5("This calculator was prepared using ARIC, CHS, CARDIA, and Framingham Study research materials obtained from the NHLBI Biologic Specimen and Data Repository Information Coordinating Center and does not necessary reflect the opinions or views of the CHS, CARDIA, Framingham Study, or the NHLBI."),
                         
                         br(),
                         h5("Calculations reported here were supported by the National Institute On Minority Health And Health Disparities of the National Institutes of Health under Award Numbers DP2MD010478 and U54MD010724, and by the National Heart, Lung and Blood Institute of the National Institutes of Health under Award Number K08HL121056. The content is solely the responsibility of the authors and does not necessarily represent the official views of the National Institutes of Health. Steve Yadlowsky is supported by the Stanford University Graduate Fellowship."),
                         
                         br(),
                         h5("The Jackson Heart Study is supported by contracts HHSN268201300046C, HHSN268201300047C, HHSN268201300048C, HHSN268201300049C, HHSN268201300050C from the National Heart, Lung, and Blood Institute and the National Institute on Minority Health and Health Disparities, with additional support from the National Institute on Biomedical Imaging and Bioengineering. "),
                         
                         br(),
                         h5("The MESA Study is supported by contracts HHSN268201500003I, N01-HC-95159, N01-HC-95160, N01-HC-95161, N01-HC-95162, N01-HC-95163, N01-HC-95164, N01-HC-95165, N01-HC-95166, N01-HC-95167, N01-HC-95168 and N01-HC-95169 from the National Heart, Lung, and Blood Institute, and by grants UL1-TR-000040, UL1-TR-001079, and UL1-TR-001420 from NCATS.  The authors thank the other investigators, the staff, and the participants of the MESA study for their valuable contributions.  A full list of participating MESA investigators and institutions can be found at http://www.mesa-nhlbi.org.")
                       
                         )

                )

server = function(input, output) {
  # Access input values with input$*
  # Save output objects to output$*
  # Build objects with render*({ code })
  
  ###----------------------------- HTE on CVD outcome  --------------------------###
  HTE_estimator_CVD = reactive({
    
    data <- data.frame(input$SBP, input$DBP, input$N_AGENTS, input$ASPIRIN, input$EGFR, input$SCREAT, input$SUB_CKD, input$RACE_BLACK,
                       input$AGE, input$FEMALE, input$SUB_CLINICALCVD, input$CHR, input$GLUR, input$HDL, input$TRR, input$BMI, 
                       input$STATIN, input$Hispanic, input$currentsmoker, input$formersmoker, input$neversmoker, input$ACE_ARB,
                       input$CCB, input$ThiazideDiuretic, input$LoopDiuretic, input$BetaBlocker, input$AlphaOneBlocker,
                       input$OtherAntiHyperMed, input$RESULT_K, input$UMALCR, input$CORONARYREVAS, input$SEATHEART, input$ULCER,
                       input$LIVEWITHOTHERS, input$DEPRESS, input$ALCOHOL, input$insurance)
    
    colnames(data) <- c("SBP","DBP","N_AGENTS","ASPIRIN","EGFR","SCREAT","SUB_CKD",          
                        "RACE_BLACK","AGE","FEMALE","SUB_CLINICALCVD","CHR","GLUR","HDL",              
                        "TRR","BMI","STATIN","Hispanic","currentsmoker","formersmoker","neversmoker",      
                        "ACE_ARB","CCB","ThiazideDiuretic","LoopDiuretic","BetaBlocker","AlphaOneBlocker","OtherAntiHyperMed",
                        "RESULT_K","UMALCR","CORONARYREVAS","SEATHEART","ULCER","LIVEWITHOTHERS","DEPRESS",          
                        "ALCOHOL","insurance")
    
    HTE_treated <- predict(fit1_cvd, data)$predictions
    HTE_control <- predict(fit0_cvd, data)$predictions
    
    HTE <- round(-(HTE_treated + HTE_control)/2,4)
    HTE
  })
  
  
  ###----------------------------- HTE on SAE outcome  --------------------------###
  HTE_estimator_SAE = reactive({
    
    data <- data.frame(input$SBP, input$DBP, input$N_AGENTS, input$ASPIRIN, input$EGFR, input$SCREAT, input$SUB_CKD, input$RACE_BLACK,
                       input$AGE, input$FEMALE, input$SUB_CLINICALCVD, input$CHR, input$GLUR, input$HDL, input$TRR, input$BMI, 
                       input$STATIN, input$Hispanic, input$currentsmoker, input$formersmoker, input$neversmoker, input$ACE_ARB,
                       input$CCB, input$ThiazideDiuretic, input$LoopDiuretic, input$BetaBlocker, input$AlphaOneBlocker,
                       input$OtherAntiHyperMed, input$RESULT_K, input$UMALCR, input$CORONARYREVAS, input$SEATHEART, input$ULCER,
                       input$LIVEWITHOTHERS, input$DEPRESS, input$ALCOHOL, input$insurance)
    
    colnames(data) <- c("SBP","DBP","N_AGENTS","ASPIRIN","EGFR","SCREAT","SUB_CKD",          
                        "RACE_BLACK","AGE","FEMALE","SUB_CLINICALCVD","CHR","GLUR","HDL",              
                        "TRR","BMI","STATIN","Hispanic","currentsmoker","formersmoker","neversmoker",      
                        "ACE_ARB","CCB","ThiazideDiuretic","LoopDiuretic","BetaBlocker","AlphaOneBlocker","OtherAntiHyperMed",
                        "RESULT_K","UMALCR","CORONARYREVAS","SEATHEART","ULCER","LIVEWITHOTHERS","DEPRESS",          
                        "ALCOHOL","insurance")
    
    HTE_treated <- predict(fit1_sae, data)$predictions
    HTE_control <- predict(fit0_sae, data)$predictions
    
    HTE <- round((HTE_treated + HTE_control)/2,4)
    HTE
  })
  
  output$HTE_CVD = renderText({ HTE_estimator_CVD() })
  output$HTE_SAE = renderText({ HTE_estimator_SAE() })
  
}

shinyApp(ui = ui, server = server)

