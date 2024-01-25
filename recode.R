#### main ###

### open libaries ###
library(haven)
library(tidyr)
library(tidyverse)
library(magrittr)
library(dplyr)
library(ggplot2)
library(psych)
library(GPArotation)
library(lm.beta)
library(mctest)
library(plotly)
library(dplyr)
library(stringr)
#library(ingmarkdowntemplates)
library(ggplot2)
library(lubridate)
library(png)
library(ggimage)
library(readxl)
library(tidyxl)
library(data.table)
library(zoo)
library(shiny)
library(corrr)
library(tidyverse)
library(caret)
library(car)
library(gt)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(shiny)
library(shinydashboard)
library(cld3)
library(Rcpp)
library(tidytext)
library(textdata)
library(sentimentr)
library(topicmodels)
#library(translateR)
library(float)
library(textmineR)
library(text2vec)
library(googleLanguageR)
library(googleAuthR)
library(devtools)
library(deeplr)
library(dplyr)
library(tidytext)
library(janeaustenr)
library(readr)
library(readxl)

DMA23 <- read_excel("~/Documents/DMA/data/Design Maturity Assessment - 2023(1-100).xlsx")
DMA22 <- read_excel("~/Documents/DMA/data/Design Maturity Assessment - 20226d315e372aff27d0368186fc8c963780360e0d19f611417cc47c0bd2b4b5edaa.xlsx", sheet="Raw data")

#DMA23 <- write_csv(DMA23, "DMA23.csv")
#DMA22 <- write_csv(DMA22, "DMA22.csv")



DMA23$Year <- "2023"
DMA22$Year <- "2022"

DMA <- full_join(DMA22, DMA23)

DMA1 <- DMA %>%
  dplyr::mutate(
    review_frequency = case_when(
      `How frequently do you review your work and get feedback from your Design team? `
      == "Never" ~ 1,
      `How frequently do you review your work and get feedback from your Design team? `
      == "On an ad-hoc basis, there is no regular frequency" ~ 2,
      `How frequently do you review your work and get feedback from your Design team? `
      == "At a set frequency, e.g. every 1-2 weeks" ~ 3,
      TRUE ~ 1)
  )

DMA14 <- DMA %>%
  dplyr::select(contains("Where are you")) 
colnames(DMA14)[1] <- "country"
DMA1$country <- DMA14$country


DMA15 <- DMA1 %>%
  dplyr::select(contains("all the roles"))
colnames(DMA15)[1] <- "roles"
DMA1$roles <- DMA15$roles 

DMA1 <- DMA1 %>%
  tidyr::separate(
    roles,
    c("ro1", "ro2", "ro3", "ro4", "ro5", "ro6", "ro7", "ro8"),
    sep = ";"
  ) %>%
  rowwise() %>%
  mutate(na_count = sum(is.na(c_across(all_of(c("ro1", "ro2", "ro3", "ro4", 
                                                "ro5", "ro6", "ro7", "ro8")))))) %>%
  dplyr::mutate(roles = 8 - na_count)

# multiple answers are not split yet
DMA1 <- DMA1 %>%
  dplyr::mutate(
    Interaction_UX_designer = case_when(
      ro1 == "Interaction Designer / UX Designer"|
        ro2 == "Interaction Designer / UX Designer"|
        ro3 == "Interaction Designer / UX Designer"|
        ro4 == "Interaction Designer / UX Designer"|
        ro5 == "Interaction Designer / UX Designer"| 
        ro6 == "Interaction Designer / UX Designer"|
        ro7 == "Interaction Designer / UX Designer"|
        ro8 == "Interaction Designer / UX Designer" ~ 1
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(
    Service_designer = case_when(
      ro1 == "Service Designer"|
        ro2 == "Service Designer"|
        ro3 == "Service Designer"|
        ro4 == "Service Designer"|
        ro5 == "Service Designer"| 
        ro6 == "Service Designer"|
        ro7 == "Service Designer"|
        ro8 == "Service Designer" ~ 1, 
    ))      

DMA1 <- DMA1 %>%
  dplyr::mutate(
    UX_researcher = case_when(
      ro1 == "UX Researcher/ User Researcher"|
        ro2 == "UX Researcher/ User Researcher"|
        ro3 == "UX Researcher/ User Researcher"|
        ro4 == "UX Researcher/ User Researcher"|
        ro5 == "UX Researcher/ User Researcher"| 
        ro6 == "UX Researcher/ User Researcher"|
        ro7 == "UX Researcher/ User Researcher"|
        ro8 == "UX Researcher/ User Researcher" ~ 1,
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(      
    UX_writer = case_when(
      ro1 == "UX Writer/ Content Designers"|
        ro2 == "UX Writer/ Content Designers"|
        ro3 == "UX Writer/ Content Designers"|
        ro4 == "UX Writer/ Content Designers"|
        ro5 == "UX Writer/ Content Designers"| 
        ro6 == "UX Writer/ Content Designers"|
        ro7 == "UX Writer/ Content Designers"|
        ro8 == "UX Writer/ Content Designers" ~ 1, 
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(   
    UI_designer = case_when(
      ro1 == "UI Designer/ Visual Designer"|
        ro2 == "UI Designer/ Visual Designer"|
        ro3 == "UI Designer/ Visual Designer"|
        ro4 == "UI Designer/ Visual Designer"|
        ro5 == "UI Designer/ Visual Designer"| 
        ro6 == "UI Designer/ Visual Designer"|
        ro7 == "UI Designer/ Visual Designer"|
        ro8 == "UI Designer/ Visual Designer" ~ 1,  
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    Chapter_lead = case_when(
      ro1 == "Design Chapter Lead"|
        ro2 == "Design Chapter Lead"|
        ro3 == "Design Chapter Lead"|
        ro4 == "Design Chapter Lead"|
        ro5 == "Design Chapter Lead"| 
        ro6 == "Design Chapter Lead"|
        ro7 == "Design Chapter Lead"|
        ro8 == "Design Chapter Lead" ~ 1,  
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    COE_lead = case_when(
      ro1 == "CoE Lead"|
        ro2 == "CoE Lead"|
        ro3 == "CoE Lead"|
        ro4 == "CoE Lead"|
        ro5 == "CoE Lead"| 
        ro6 == "CoE Lead"|
        ro7 == "CoE Lead"|
        ro8 == "CoE Lead" ~ 1,   
    ))


DMA18 <- DMA1 %>%
  dplyr::select(contains("which statement best describes when your Design team is involved")) 
colnames(DMA18)[1] <- "involvement"
DMA1$involvement <- DMA18$involvement


DMA1 <- DMA1 %>%
  dplyr::mutate(
    design_involvement = case_when(
      involvement=="The Design Team is rarely involved prior to when development has started " ~ 1,
      involvement=="The Design Team is rarely involved prior to when development has started" ~ 1,
      involvement=="The Design Team is sometimes involved prior to development, but doesn't always have enough time to test and iterate " ~ 2,
      involvement=="The Design Team is sometimes involved prior to when development has started" ~ 2,
      involvement=="The Design Team is involved prior to when development starts  " ~ 3,
      involvement=="I’m not sure" ~ 1,
      involvement=="The Design Team is mostly involved after development has started" ~ 3,
      involvement=="The Design Team is often involved prior to development, with enough time to fill a healthy backlog" ~ 4,
      involvement=="The Design Team is involved early enough to fill a healthy backlog. The Design team helps form the business & market requirements" ~ 4,
      involvement=="The Design Team is always involved early enough to fill a healthy backlog. The Design Team also helps form the business & market requirements" ~ 5,
      startsWith(as.character(involvement), "The Design Team is rarely") ~ 1,
      startsWith(as.character(involvement), "The Design Team is sometimes") ~ 2,
      startsWith(as.character(involvement), "The Design Team is involved prior") ~ 3,
      startsWith(as.character(involvement), "The Design Team is mostly involved") ~ 3,
      startsWith(as.character(involvement), "The Design Team is often involved") ~ 4,
      startsWith(as.character(involvement), "The Design Team is involved early") ~ 4,
      startsWith(as.character(involvement), "The Design Team is always") ~ 5,
      #TRUE ~ 1
      )
  )

DMA1 <- DMA1 %>%
  dplyr::mutate(
    desired_workflow = case_when(
      `For the majority of your projects, are you involved early enough to be able to follow your desired workflow?\r\n`
      == "Never, I always need to compromise to meet a deadline" ~ 1,
      `For the majority of your projects, are you involved early enough to be able to follow your desired workflow?\r\n`
        =="Rarely, I can follow my desired workflow for around 25% of projects" ~ 2,
      `For the majority of your projects, are you involved early enough to be able to follow your desired workflow?\r\n`
        =="I can follow my desired workflow for around 25% of projects" ~ 2,
      `For the majority of your projects, are you involved early enough to be able to follow your desired workflow?\r\n`
        =="Sometimes, I can follow my desired workflow for around 50% of projects" ~ 3,
      `For the majority of your projects, are you involved early enough to be able to follow your desired workflow?\r\n`
        =="I can follow my desired workflow for around 50% of projects" ~ 3,
      `For the majority of your projects, are you involved early enough to be able to follow your desired workflow?\r\n`
        =="I can follow my desired workflow for around 75% of projects" ~ 4,
      `For the majority of your projects, are you involved early enough to be able to follow your desired workflow?\r\n`
        =="Often, I can follow my desired workflow for around 75% of projects" ~ 4,
      `For the majority of your projects, are you involved early enough to be able to follow your desired workflow?\r\n`
        =="I am always involved early enough to follow my desired workflow" ~ 5,
      `For the majority of your projects, are you involved early enough to be able to follow your desired workflow?\r\n`
        =="Always, I am always involved early enough to follow my desired workflow" ~ 5,
      startsWith(as.character(`For the majority of your projects, are you involved early enough to be able to follow your desired workflow?\r\n`), "Never") ~ 1,
      startsWith(as.character(`For the majority of your projects, are you involved early enough to be able to follow your desired workflow?\r\n`), "Rarely") ~ 2,
     # TRUE ~ 1
       )  
  )

    
DMA1 <- DMA1 %>%
  dplyr::mutate(
    training = case_when(
      `Do you agree.... every year you receive enough training to keep improving in your job...` ==
        "Disagree" ~ 1, 
      `Do you agree.... every year you receive enough training to keep improving in your job...` ==
        "Some what disagree" ~ 2, 
      `Do you agree.... every year you receive enough training to keep improving in your job...` ==
        "Neither agree or disagree" ~ 3,
      `Do you agree.... every year you receive enough training to keep improving in your job...` ==
        "Somewhat agree" ~ 4, 
      `Do you agree.... every year you receive enough training to keep improving in your job...` ==
        "Agree" ~ 5,
      TRUE ~ 1)
  )
    

DMA1 <- DMA1 %>%
  tidyr::separate(
    `Which UX research methods do you often use in most of your projects?`,
    c("ux1", "ux2", "ux3", "ux4", "ux5", "ux6", "ux7", "ux8", "ux9", "ux10",
      "ux11", "ux12", "ux13", "ux14", "ux15", "ux16", "ux17", "ux18", "ux19", "ux20",
      "ux21"),
    sep = ";"
  ) %>%
  rowwise() %>%
  mutate(na_count = sum(is.na(c_across(all_of(c("ux1", "ux2", "ux3", "ux4",
                                                "ux5", "ux6","ux7", "ux8",
                                                "ux9", "ux10","ux11", "ux12",
                                                "ux13", "ux14","ux15", "ux16",
                                                "ux17", "ux18","ux19", "ux20",
                                                "ux21")))))) %>%
  dplyr::mutate(UX_applied = 21 - na_count)

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    Accessibility_studies = case_when(
      ux1 == "Accessibility studies with an automated tool, participants, or experts"|
      ux2 == "Accessibility studies with an automated tool, participants, or experts"|
      ux3 == "Accessibility studies with an automated tool, participants, or experts"|
      ux4 == "Accessibility studies with an automated tool, participants, or experts"|
      ux5 == "Accessibility studies with an automated tool, participants, or experts"|
      ux6 == "Accessibility studies with an automated tool, participants, or experts"|
      ux7 == "Accessibility studies with an automated tool, participants, or experts"|
      ux8 == "Accessibility studies with an automated tool, participants, or experts"|
      ux9 == "Accessibility studies with an automated tool, participants, or experts"|
      ux10 == "Accessibility studies with an automated tool, participants, or experts"|
      ux11 == "Accessibility studies with an automated tool, participants, or experts"|
      ux12 == "Accessibility studies with an automated tool, participants, or experts"|
      ux13 == "Accessibility studies with an automated tool, participants, or experts"|
      ux14 == "Accessibility studies with an automated tool, participants, or experts"|  
      ux15 == "Accessibility studies with an automated tool, participants, or experts"|
      ux16 == "Accessibility studies with an automated tool, participants, or experts"|
      ux17 == "Accessibility studies with an automated tool, participants, or experts"|
      ux18 == "Accessibility studies with an automated tool, participants, or experts"|
      ux19 == "Accessibility studies with an automated tool, participants, or experts"|
      ux20 == "Accessibility studies with an automated tool, participants, or experts"|
      ux21 == "Accessibility studies with an automated tool, participants, or experts" ~ 1,   
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    Business_Product_Analytics_Review = case_when(
      ux1 == "Business and Product Analytics Review"|
        ux2 == "Business and Product Analytics Review"|
        ux3 == "Business and Product Analytics Review"|
        ux4 == "Business and Product Analytics Review"|
        ux5 == "Business and Product Analytics Review"|
        ux6 == "Business and Product Analytics Review"|
        ux7 == "Business and Product Analytics Review"|
        ux8 == "Business and Product Analytics Review"|
        ux9 == "Business and Product Analytics Review"|
        ux10 == "Business and Product Analytics Review"|
        ux11 == "Business and Product Analytics Review"|
        ux12 == "Business and Product Analytics Review"|
        ux13 == "Business and Product Analytics Review"|
        ux14 == "Business and Product Analytics Review"|  
        ux15 == "Business and Product Analytics Review"|
        ux16 == "Business and Product Analytics Review"|
        ux17 == "Business and Product Analytics Review"|
        ux18 == "Business and Product Analytics Review"|
        ux19 == "Business and Product Analytics Review"|
        ux20 == "Business and Product Analytics Review"|
        ux21 == "Business and Product Analytics Review" ~ 1,   
    ))


DMA1 <- DMA1 %>%
  dplyr::mutate(  
    Competitor_analysis = case_when(
      ux1 == "Competitor Analysis"|
        ux2 == "Competitor Analysis"|
        ux3 == "Competitor Analysis"|
        ux4 == "Competitor Analysis"|
        ux5 == "Competitor Analysis"|
        ux6 == "Competitor Analysis"|
        ux7 == "Competitor Analysis"|
        ux8 == "Competitor Analysis"|
        ux9 == "Competitor Analysis"|
        ux10 == "Competitor Analysis"|
        ux11 == "Competitor Analysis"|
        ux12 == "Competitor Analysis"|
        ux13 == "Competitor Analysis"|
        ux14 == "Competitor Analysis"|  
        ux15 == "Competitor Analysis"|
        ux16 == "Competitor Analysis"|
        ux17 == "Competitor Analysis"|
        ux18 == "Competitor Analysis"|
        ux19 == "Competitor Analysis"|
        ux20 == "Competitor Analysis"|
        ux21 == "Competitor Analysis" ~ 1,   
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(  
   Concept_testing = case_when(
      ux1 == "Concept Testing"|
        ux2 == "Concept Testing"|
        ux3 == "Concept Testing"|
        ux4 == "Concept Testing"|
        ux5 == "Concept Testing"|
        ux6 == "Concept Testing"|
        ux7 == "Concept Testing"|
        ux8 == "Concept Testing"|
        ux9 == "Concept Testing"|
        ux10 == "Concept Testing"|
        ux11 == "Concept Testing"|
        ux12 == "Concept Testing"|
        ux13 == "Concept Testing"|
        ux14 == "Concept Testing"|  
        ux15 == "Concept Testing"|
        ux16 == "Concept Testing"|
        ux17 == "Concept Testing"|
        ux18 == "Concept Testing"|
        ux19 == "Concept Testing"|
        ux20 == "Concept Testing"|
        ux21 == "Concept Testing" ~ 1,   
    ))


DMA1 <- DMA1 %>%
  dplyr::mutate(  
    Customer_journey_mapping = case_when(
      ux1 == "Customer Journey Mapping"|
        ux2 == "Customer Journey Mapping"|
        ux3 == "Customer Journey Mapping"|
        ux4 == "Customer Journey Mapping"|
        ux5 == "Customer Journey Mapping"|
        ux6 == "Customer Journey Mapping"|
        ux7 == "Customer Journey Mapping"|
        ux8 == "Customer Journey Mapping"|
        ux9 == "Customer Journey Mapping"|
        ux10 == "Customer Journey Mapping"|
        ux11 == "Customer Journey Mapping"|
        ux12 == "Customer Journey Mapping"|
        ux13 == "Customer Journey Mapping"|
        ux14 == "Customer Journey Mapping"|  
        ux15 == "Customer Journey Mapping"|
        ux16 == "Customer Journey Mapping"|
        ux17 == "Customer Journey Mapping"|
        ux18 == "Customer Journey Mapping"|
        ux19 == "Customer Journey Mapping"|
        ux20 == "Customer Journey Mapping"|
        ux21 == "Customer Journey Mapping" ~ 1,   
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    Field_research = case_when(
      ux1 == "Field research (Contextual Inquiry, observations, ethnographic studies etc.)"|
        ux2 == "Field research (Contextual Inquiry, observations, ethnographic studies etc.)"|
        ux3 == "Field research (Contextual Inquiry, observations, ethnographic studies etc.)"|
        ux4 == "Field research (Contextual Inquiry, observations, ethnographic studies etc.)"|
        ux5 == "Field research (Contextual Inquiry, observations, ethnographic studies etc.)"|
        ux6 == "Field research (Contextual Inquiry, observations, ethnographic studies etc.)"|
        ux7 == "Field research (Contextual Inquiry, observations, ethnographic studies etc.)"|
        ux8 == "Field research (Contextual Inquiry, observations, ethnographic studies etc.)"|
        ux9 == "Field research (Contextual Inquiry, observations, ethnographic studies etc.)"|
        ux10 == "Field research (Contextual Inquiry, observations, ethnographic studies etc.)"|
        ux11 == "Field research (Contextual Inquiry, observations, ethnographic studies etc.)"|
        ux12 == "Field research (Contextual Inquiry, observations, ethnographic studies etc.)"|
        ux13 == "Field research (Contextual Inquiry, observations, ethnographic studies etc.)"|
        ux14 == "Field research (Contextual Inquiry, observations, ethnographic studies etc.)"|  
        ux15 == "Field research (Contextual Inquiry, observations, ethnographic studies etc.)"|
        ux16 == "Field research (Contextual Inquiry, observations, ethnographic studies etc.)"|
        ux17 == "Field research (Contextual Inquiry, observations, ethnographic studies etc.)"|
        ux18 == "Field research (Contextual Inquiry, observations, ethnographic studies etc.)"|
        ux19 == "Field research (Contextual Inquiry, observations, ethnographic studies etc.)"|
        ux20 == "Field research (Contextual Inquiry, observations, ethnographic studies etc.)"|
        ux21 == "Field research (Contextual Inquiry, observations, ethnographic studies etc.)" ~ 1,   
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    Existing_research = case_when(
      ux1 == "Review of existing research studies (Internal or external sources)"|
        ux2 == "Review of existing research studies (Internal or external sources)"|
        ux3 == "Review of existing research studies (Internal or external sources)"|
        ux4 == "Review of existing research studies (Internal or external sources)"|
        ux5 == "Review of existing research studies (Internal or external sources)"|
        ux6 == "Review of existing research studies (Internal or external sources)"|
        ux7 == "Review of existing research studies (Internal or external sources)"|
        ux8 == "Review of existing research studies (Internal or external sources)"|
        ux9 == "Review of existing research studies (Internal or external sources)"|
        ux10 == "Review of existing research studies (Internal or external sources)"|
        ux11 == "Review of existing research studies (Internal or external sources)"|
        ux12 == "Review of existing research studies (Internal or external sources)"|
        ux13 == "Review of existing research studies (Internal or external sources)"|
        ux14 == "Review of existing research studies (Internal or external sources)"|  
        ux15 == "Review of existing research studies (Internal or external sources)"|
        ux16 == "Review of existing research studies (Internal or external sources)"|
        ux17 == "Review of existing research studies (Internal or external sources)"|
        ux18 == "Review of existing research studies (Internal or external sources)"|
        ux19 == "Review of existing research studies (Internal or external sources)"|
        ux20 == "Review of existing research studies (Internal or external sources)"|
        ux21 == "Review of existing research studies (Internal or external sources)" ~ 1,   
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    Experimentation = case_when(
      ux1 == "Experimentation (Launching a feature in a controlled group, A/B Testing, and Multivariate Testing.)"|
        ux2 == "Experimentation (Launching a feature in a controlled group, A/B Testing, and Multivariate Testing.)"|
        ux3 == "Experimentation (Launching a feature in a controlled group, A/B Testing, and Multivariate Testing.)"|
        ux4 == "Experimentation (Launching a feature in a controlled group, A/B Testing, and Multivariate Testing.)"|
        ux5 == "Experimentation (Launching a feature in a controlled group, A/B Testing, and Multivariate Testing.)"|
        ux6 == "Experimentation (Launching a feature in a controlled group, A/B Testing, and Multivariate Testing.)"|
        ux7 == "Experimentation (Launching a feature in a controlled group, A/B Testing, and Multivariate Testing.)"|
        ux8 == "Experimentation (Launching a feature in a controlled group, A/B Testing, and Multivariate Testing.)"|
        ux9 == "Experimentation (Launching a feature in a controlled group, A/B Testing, and Multivariate Testing.)"|
        ux10 == "Experimentation (Launching a feature in a controlled group, A/B Testing, and Multivariate Testing.)"|
        ux11 == "Experimentation (Launching a feature in a controlled group, A/B Testing, and Multivariate Testing.)"|
        ux12 == "Experimentation (Launching a feature in a controlled group, A/B Testing, and Multivariate Testing.)"|
        ux13 == "Experimentation (Launching a feature in a controlled group, A/B Testing, and Multivariate Testing.)"|
        ux14 == "Experimentation (Launching a feature in a controlled group, A/B Testing, and Multivariate Testing.)"|  
        ux15 == "Experimentation (Launching a feature in a controlled group, A/B Testing, and Multivariate Testing.)"|
        ux16 == "Experimentation (Launching a feature in a controlled group, A/B Testing, and Multivariate Testing.)"|
        ux17 == "Experimentation (Launching a feature in a controlled group, A/B Testing, and Multivariate Testing.)"|
        ux18 == "Experimentation (Launching a feature in a controlled group, A/B Testing, and Multivariate Testing.)"|
        ux19 == "Experimentation (Launching a feature in a controlled group, A/B Testing, and Multivariate Testing.)"|
        ux20 == "Experimentation (Launching a feature in a controlled group, A/B Testing, and Multivariate Testing.)"|
        ux21 == "Experimentation (Launching a feature in a controlled group, A/B Testing, and Multivariate Testing.)" ~ 1,   
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    Expert_reviews = case_when(
      ux1 == "Expert Reviews (Usability Audit, Heuristic Evaluations, Cognitive Walkthroughs, PURE etc.)"|
        ux2 == "Expert Reviews (Usability Audit, Heuristic Evaluations, Cognitive Walkthroughs, PURE etc.)"|
        ux3 == "Expert Reviews (Usability Audit, Heuristic Evaluations, Cognitive Walkthroughs, PURE etc.)"|
        ux4 == "Expert Reviews (Usability Audit, Heuristic Evaluations, Cognitive Walkthroughs, PURE etc.)"|
        ux5 == "Expert Reviews (Usability Audit, Heuristic Evaluations, Cognitive Walkthroughs, PURE etc.)"|
        ux6 == "Expert Reviews (Usability Audit, Heuristic Evaluations, Cognitive Walkthroughs, PURE etc.)"|
        ux7 == "Expert Reviews (Usability Audit, Heuristic Evaluations, Cognitive Walkthroughs, PURE etc.)"|
        ux8 == "Expert Reviews (Usability Audit, Heuristic Evaluations, Cognitive Walkthroughs, PURE etc.)"|
        ux9 == "Expert Reviews (Usability Audit, Heuristic Evaluations, Cognitive Walkthroughs, PURE etc.)"|
        ux10 == "Expert Reviews (Usability Audit, Heuristic Evaluations, Cognitive Walkthroughs, PURE etc.)"|
        ux11 == "Expert Reviews (Usability Audit, Heuristic Evaluations, Cognitive Walkthroughs, PURE etc.)"|
        ux12 == "Expert Reviews (Usability Audit, Heuristic Evaluations, Cognitive Walkthroughs, PURE etc.)"|
        ux13 == "Expert Reviews (Usability Audit, Heuristic Evaluations, Cognitive Walkthroughs, PURE etc.)"|
        ux14 == "Expert Reviews (Usability Audit, Heuristic Evaluations, Cognitive Walkthroughs, PURE etc.)"|  
        ux15 == "Expert Reviews (Usability Audit, Heuristic Evaluations, Cognitive Walkthroughs, PURE etc.)"|
        ux16 == "Expert Reviews (Usability Audit, Heuristic Evaluations, Cognitive Walkthroughs, PURE etc.)"|
        ux17 == "Expert Reviews (Usability Audit, Heuristic Evaluations, Cognitive Walkthroughs, PURE etc.)"|
        ux18 == "Expert Reviews (Usability Audit, Heuristic Evaluations, Cognitive Walkthroughs, PURE etc.)"|
        ux19 == "Expert Reviews (Usability Audit, Heuristic Evaluations, Cognitive Walkthroughs, PURE etc.)"|
        ux20 == "Expert Reviews (Usability Audit, Heuristic Evaluations, Cognitive Walkthroughs, PURE etc.)"|
        ux21 == "Expert Reviews (Usability Audit, Heuristic Evaluations, Cognitive Walkthroughs, PURE etc.)" ~ 1,   
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    Focus_groups = case_when(
      ux1 == "Focus Group Discussions"|
        ux2 == "Focus Group Discussions"|
        ux3 == "Focus Group Discussions"|
        ux4 == "Focus Group Discussions"|
        ux5 == "Focus Group Discussions"|
        ux6 == "Focus Group Discussions"|
        ux7 == "Focus Group Discussions"|
        ux8 == "Focus Group Discussions"|
        ux9 == "Focus Group Discussions"|
        ux10 == "Focus Group Discussions"|
        ux11 == "Focus Group Discussions"|
        ux12 == "Focus Group Discussions"|
        ux13 == "Focus Group Discussions"|
        ux14 == "Focus Group Discussions"|  
        ux15 == "Focus Group Discussions"|
        ux16 == "Focus Group Discussions"|
        ux17 == "Focus Group Discussions"|
        ux18 == "Focus Group Discussions"|
        ux19 == "Focus Group Discussions"|
        ux20 == "Focus Group Discussions"|
        ux21 == "Focus Group Discussions" ~ 1,   
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    Interviews = case_when(
      ux1 == "In-depth Interviews"|
        ux2 == "In-depth Interviews"|
        ux3 == "In-depth Interviews"|
        ux4 == "In-depth Interviews"|
        ux5 == "In-depth Interviews"|
        ux6 == "In-depth Interviews"|
        ux7 == "In-depth Interviews"|
        ux8 == "In-depth Interviews"|
        ux9 == "In-depth Interviews"|
        ux10 == "In-depth Interviews"|
        ux11 == "In-depth Interviews"|
        ux12 == "In-depth Interviews"|
        ux13 == "In-depth Interviews"|
        ux14 == "In-depth Interviews"|  
        ux15 == "In-depth Interviews"|
        ux16 == "In-depth Interviews"|
        ux17 == "In-depth Interviews"|
        ux18 == "In-depth Interviews"|
        ux19 == "In-depth Interviews"|
        ux20 == "In-depth Interviews"|
        ux21 == "In-depth Interviews" ~ 1,   
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    Information_Architecture = case_when(
      ux1 == "Information Architecture Research (Open or closed card sorting, Tree testing, etc.)"|
        ux2 == "Information Architecture Research (Open or closed card sorting, Tree testing, etc.)"|
        ux3 == "Information Architecture Research (Open or closed card sorting, Tree testing, etc.)"|
        ux4 == "Information Architecture Research (Open or closed card sorting, Tree testing, etc.)"|
        ux5 == "Information Architecture Research (Open or closed card sorting, Tree testing, etc.)"|
        ux6 == "Information Architecture Research (Open or closed card sorting, Tree testing, etc.)"|
        ux7 == "Information Architecture Research (Open or closed card sorting, Tree testing, etc.)"|
        ux8 == "Information Architecture Research (Open or closed card sorting, Tree testing, etc.)"|
        ux9 == "Information Architecture Research (Open or closed card sorting, Tree testing, etc.)"|
        ux10 == "Information Architecture Research (Open or closed card sorting, Tree testing, etc.)"|
        ux11 == "Information Architecture Research (Open or closed card sorting, Tree testing, etc.)"|
        ux12 == "Information Architecture Research (Open or closed card sorting, Tree testing, etc.)"|
        ux13 == "Information Architecture Research (Open or closed card sorting, Tree testing, etc.)"|
        ux14 == "Information Architecture Research (Open or closed card sorting, Tree testing, etc.)"|  
        ux15 == "Information Architecture Research (Open or closed card sorting, Tree testing, etc.)"|
        ux16 == "Information Architecture Research (Open or closed card sorting, Tree testing, etc.)"|
        ux17 == "Information Architecture Research (Open or closed card sorting, Tree testing, etc.)"|
        ux18 == "Information Architecture Research (Open or closed card sorting, Tree testing, etc.)"|
        ux19 == "Information Architecture Research (Open or closed card sorting, Tree testing, etc.)"|
        ux20 == "Information Architecture Research (Open or closed card sorting, Tree testing, etc.)"|
        ux21 == "Information Architecture Research (Open or closed card sorting, Tree testing, etc.)" ~ 1,   
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    Persona = case_when(
      ux1 == "Persona Creation"|
        ux2 == "Persona Creation"|
        ux3 == "Persona Creation"|
        ux4 == "Persona Creation"|
        ux5 == "Persona Creation"|
        ux6 == "Persona Creation"|
        ux7 == "Persona Creation"|
        ux8 == "Persona Creation"|
        ux9 == "Persona Creation"|
        ux10 == "Persona Creation"|
        ux11 == "Persona Creation"|
        ux12 == "Persona Creation"|
        ux13 == "Persona Creation"|
        ux14 == "Persona Creation"|  
        ux15 == "Persona Creation"|
        ux16 == "Persona Creation"|
        ux17 == "Persona Creation"|
        ux18 == "Persona Creation"|
        ux19 == "Persona Creation"|
        ux20 == "Persona Creation"|
        ux21 == "Persona Creation" ~ 1,   
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    Quali_Usability_testing = case_when(
      ux1 == "Qualitative Usability Test (Moderated test in-lab or own environment)"|
        ux2 == "Qualitative Usability Test (Moderated test in-lab or own environment)"|
        ux3 == "Qualitative Usability Test (Moderated test in-lab or own environment)"|
        ux4 == "Qualitative Usability Test (Moderated test in-lab or own environment)"|
        ux5 == "Qualitative Usability Test (Moderated test in-lab or own environment)"|
        ux6 == "Qualitative Usability Test (Moderated test in-lab or own environment)"|
        ux7 == "Qualitative Usability Test (Moderated test in-lab or own environment)"|
        ux8 == "Qualitative Usability Test (Moderated test in-lab or own environment)"|
        ux9 == "Qualitative Usability Test (Moderated test in-lab or own environment)"|
        ux10 == "Qualitative Usability Test (Moderated test in-lab or own environment)"|
        ux11 == "Qualitative Usability Test (Moderated test in-lab or own environment)"|
        ux12 == "Qualitative Usability Test (Moderated test in-lab or own environment)"|
        ux13 == "Qualitative Usability Test (Moderated test in-lab or own environment)"|
        ux14 == "Qualitative Usability Test (Moderated test in-lab or own environment)"|  
        ux15 == "Qualitative Usability Test (Moderated test in-lab or own environment)"|
        ux16 == "Qualitative Usability Test (Moderated test in-lab or own environment)"|
        ux17 == "Qualitative Usability Test (Moderated test in-lab or own environment)"|
        ux18 == "Qualitative Usability Test (Moderated test in-lab or own environment)"|
        ux19 == "Qualitative Usability Test (Moderated test in-lab or own environment)"|
        ux20 == "Qualitative Usability Test (Moderated test in-lab or own environment)"|
        ux21 == "Qualitative Usability Test (Moderated test in-lab or own environment)" ~ 1,   
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    Quanti_Attitudinal_test = case_when(
      ux1 == "Quantitative Attitudinal Test (Preference Test, 5-seconds test, desirability studies, etc.)"|
        ux2 == "Quantitative Attitudinal Test (Preference Test, 5-seconds test, desirability studies, etc.)"|
        ux3 == "Quantitative Attitudinal Test (Preference Test, 5-seconds test, desirability studies, etc.)"|
        ux4 == "Quantitative Attitudinal Test (Preference Test, 5-seconds test, desirability studies, etc.)"|
        ux5 == "Quantitative Attitudinal Test (Preference Test, 5-seconds test, desirability studies, etc.)"|
        ux6 == "Quantitative Attitudinal Test (Preference Test, 5-seconds test, desirability studies, etc.)"|
        ux7 == "Quantitative Attitudinal Test (Preference Test, 5-seconds test, desirability studies, etc.)"|
        ux8 == "Quantitative Attitudinal Test (Preference Test, 5-seconds test, desirability studies, etc.)"|
        ux9 == "Quantitative Attitudinal Test (Preference Test, 5-seconds test, desirability studies, etc.)"|
        ux10 == "Quantitative Attitudinal Test (Preference Test, 5-seconds test, desirability studies, etc.)"|
        ux11 == "Quantitative Attitudinal Test (Preference Test, 5-seconds test, desirability studies, etc.)"|
        ux12 == "Quantitative Attitudinal Test (Preference Test, 5-seconds test, desirability studies, etc.)"|
        ux13 == "Quantitative Attitudinal Test (Preference Test, 5-seconds test, desirability studies, etc.)"|
        ux14 == "Quantitative Attitudinal Test (Preference Test, 5-seconds test, desirability studies, etc.)"|  
        ux15 == "Quantitative Attitudinal Test (Preference Test, 5-seconds test, desirability studies, etc.)"|
        ux16 == "Quantitative Attitudinal Test (Preference Test, 5-seconds test, desirability studies, etc.)"|
        ux17 == "Quantitative Attitudinal Test (Preference Test, 5-seconds test, desirability studies, etc.)"|
        ux18 == "Quantitative Attitudinal Test (Preference Test, 5-seconds test, desirability studies, etc.)"|
        ux19 == "Quantitative Attitudinal Test (Preference Test, 5-seconds test, desirability studies, etc.)"|
        ux20 == "Quantitative Attitudinal Test (Preference Test, 5-seconds test, desirability studies, etc.)"|
        ux21 == "Quantitative Attitudinal Test (Preference Test, 5-seconds test, desirability studies, etc.)" ~ 1,   
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    Quanti_Usability_testing = case_when(
      ux1 == "Quantitative Usability Test (Benchmarking, First-click test, Eye tracking etc.)"|
        ux2 == "Quantitative Usability Test (Benchmarking, First-click test, Eye tracking etc.)"|
        ux3 == "Quantitative Usability Test (Benchmarking, First-click test, Eye tracking etc.)"|
        ux4 == "Quantitative Usability Test (Benchmarking, First-click test, Eye tracking etc.)"|
        ux5 == "Quantitative Usability Test (Benchmarking, First-click test, Eye tracking etc.)"|
        ux6 == "Quantitative Usability Test (Benchmarking, First-click test, Eye tracking etc.)"|
        ux7 == "Quantitative Usability Test (Benchmarking, First-click test, Eye tracking etc.)"|
        ux8 == "Quantitative Usability Test (Benchmarking, First-click test, Eye tracking etc.)"|
        ux9 == "Quantitative Usability Test (Benchmarking, First-click test, Eye tracking etc.)"|
        ux10 == "Quantitative Usability Test (Benchmarking, First-click test, Eye tracking etc.)"|
        ux11 == "Quantitative Usability Test (Benchmarking, First-click test, Eye tracking etc.)"|
        ux12 == "Quantitative Usability Test (Benchmarking, First-click test, Eye tracking etc.)"|
        ux13 == "Quantitative Usability Test (Benchmarking, First-click test, Eye tracking etc.)"|
        ux14 == "Quantitative Usability Test (Benchmarking, First-click test, Eye tracking etc.)"|  
        ux15 == "Quantitative Usability Test (Benchmarking, First-click test, Eye tracking etc.)"|
        ux16 == "Quantitative Usability Test (Benchmarking, First-click test, Eye tracking etc.)"|
        ux17 == "Quantitative Usability Test (Benchmarking, First-click test, Eye tracking etc.)"|
        ux18 == "Quantitative Usability Test (Benchmarking, First-click test, Eye tracking etc.)"|
        ux19 == "Quantitative Usability Test (Benchmarking, First-click test, Eye tracking etc.)"|
        ux20 == "Quantitative Usability Test (Benchmarking, First-click test, Eye tracking etc.)"|
        ux21 == "Quantitative Usability Test (Benchmarking, First-click test, Eye tracking etc.)" ~ 1,   
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    Stakeholder_interviews= case_when(
      ux1 == "Stakeholder Interviews"|
        ux2 == "Stakeholder Interviews"|
        ux3 == "Stakeholder Interviews"|
        ux4 == "Stakeholder Interviews"|
        ux5 == "Stakeholder Interviews"|
        ux6 == "Stakeholder Interviews"|
        ux7 == "Stakeholder Interviews"|
        ux8 == "Stakeholder Interviews"|
        ux9 == "Stakeholder Interviews"|
        ux10 == "Stakeholder Interviews"|
        ux11 == "Stakeholder Interviews"|
        ux12 == "Stakeholder Interviews"|
        ux13 == "Stakeholder Interviews"|
        ux14 == "Stakeholder Interviews"|  
        ux15 == "Stakeholder Interviews"|
        ux16 == "Stakeholder Interviews"|
        ux17 == "Stakeholder Interviews"|
        ux18 == "Stakeholder Interviews"|
        ux19 == "Stakeholder Interviews"|
        ux20 == "Stakeholder Interviews"|
        ux21 == "Stakeholder Interviews" ~ 1,   
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    Sentiment_analysis= case_when(
      ux1 == "Sentiment Analysis (VoC, NPS, CSAT, CES, and Social Listening)"|
        ux2 == "Sentiment Analysis (VoC, NPS, CSAT, CES, and Social Listening)"|
        ux3 == "Sentiment Analysis (VoC, NPS, CSAT, CES, and Social Listening)"|
        ux4 == "Sentiment Analysis (VoC, NPS, CSAT, CES, and Social Listening)"|
        ux5 == "Sentiment Analysis (VoC, NPS, CSAT, CES, and Social Listening)"|
        ux6 == "Sentiment Analysis (VoC, NPS, CSAT, CES, and Social Listening)"|
        ux7 == "Sentiment Analysis (VoC, NPS, CSAT, CES, and Social Listening)"|
        ux8 == "Sentiment Analysis (VoC, NPS, CSAT, CES, and Social Listening)"|
        ux9 == "Sentiment Analysis (VoC, NPS, CSAT, CES, and Social Listening)"|
        ux10 == "Sentiment Analysis (VoC, NPS, CSAT, CES, and Social Listening)"|
        ux11 == "Sentiment Analysis (VoC, NPS, CSAT, CES, and Social Listening)"|
        ux12 == "Sentiment Analysis (VoC, NPS, CSAT, CES, and Social Listening)"|
        ux13 == "Sentiment Analysis (VoC, NPS, CSAT, CES, and Social Listening)"|
        ux14 == "Sentiment Analysis (VoC, NPS, CSAT, CES, and Social Listening)"|  
        ux15 == "Sentiment Analysis (VoC, NPS, CSAT, CES, and Social Listening)"|
        ux16 == "Sentiment Analysis (VoC, NPS, CSAT, CES, and Social Listening)"|
        ux17 == "Sentiment Analysis (VoC, NPS, CSAT, CES, and Social Listening)"|
        ux18 == "Sentiment Analysis (VoC, NPS, CSAT, CES, and Social Listening)"|
        ux19 == "Sentiment Analysis (VoC, NPS, CSAT, CES, and Social Listening)"|
        ux20 == "Sentiment Analysis (VoC, NPS, CSAT, CES, and Social Listening)"|
        ux21 == "Sentiment Analysis (VoC, NPS, CSAT, CES, and Social Listening)" ~ 1,   
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    Service_Blueprinting= case_when(
      ux1 == "Service Blueprinting"|
        ux2 == "Service Blueprinting"|
        ux3 == "Service Blueprinting"|
        ux4 == "Service Blueprinting"|
        ux5 == "Service Blueprinting"|
        ux6 == "Service Blueprinting"|
        ux7 == "Service Blueprinting"|
        ux8 == "Service Blueprinting"|
        ux9 == "Service Blueprinting"|
        ux10 == "Service Blueprinting"|
        ux11 == "Service Blueprinting"|
        ux12 == "Service Blueprinting"|
        ux13 == "Service Blueprinting"|
        ux14 == "Service Blueprinting"|  
        ux15 == "Service Blueprinting"|
        ux16 == "Service Blueprinting"|
        ux17 == "Service Blueprinting"|
        ux18 == "Service Blueprinting"|
        ux19 == "Service Blueprinting"|
        ux20 == "Service Blueprinting"|
        ux21 == "Service Blueprinting" ~ 1,   
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    Survey = case_when(
      ux1 == "Surveys"|
        ux2 == "Surveys"|
        ux3 == "Surveys"|
        ux4 == "Surveys"|
        ux5 == "Surveys"|
        ux6 == "Surveys"|
        ux7 == "Surveys"|
        ux8 == "Surveys"|
        ux9 == "Surveys"|
        ux10 == "Surveys"|
        ux11 == "Surveys"|
        ux12 == "Surveys"|
        ux13 == "Surveys"|
        ux14 == "Surveys"|  
        ux15 == "Surveys"|
        ux16 == "Surveys"|
        ux17 == "Surveys"|
        ux18 == "Surveys"|
        ux19 == "Surveys"|
        ux20 == "Surveys"|
        ux21 == "Surveys" ~ 1,   
    ))



DMA1 <- DMA1 %>%
  tidyr::separate(
    `Which design methods do you often use in most of your projects?`,
    c("dt1", "dt2", "dt3", "dt4", "dt5", "dt6", "dt7", "dt8", "dt9", "dt10",
      "dt11", "dt12", "dt13", "dt14", "dt15", "dt16", "dt17"),
    sep = ";"
  ) %>%
  rowwise() %>%
  mutate(na_count = sum(is.na(c_across(all_of(c("dt1", "dt2", "dt3", "dt4", "dt5", "dt6", "dt7", "dt8", "dt9", "dt10",
                                                "dt11", "dt12", "dt13", "dt14", "dt15", "dt16", "dt17")))))) %>%
  dplyr::mutate(dt_applied_points = 17 - na_count)


DMA1 <- DMA1 %>%
  dplyr::mutate(  
    Design_review = case_when(
      dt1 == "Design review"|
        dt2 == "Design review"|
        dt3 == "Design review"|
        dt4 == "Design review"|
        dt5 == "Design review"|
        dt6 == "Design review"|
        dt7 == "Design review"|
        dt8 == "Design review"|
        dt9 == "Design review"|
        dt10 == "Design review"|
        dt11 == "Design review"|
        dt12 == "Design review"|
        dt13 == "Design review"|
        dt14 == "Design review"|  
        dt15 == "Design review"|
        dt16 == "Design review"|
        dt17 == "Design review" ~ 1,   
    ))


DMA1 <- DMA1 %>%
  dplyr::mutate(  
    Ideation_workshops = case_when(
      dt1 == "Ideation workshops"|
        dt2 == "Ideation workshops"|
        dt3 == "Ideation workshops"|
        dt4 == "Ideation workshops"|
        dt5 == "Ideation workshops"|
        dt6 == "Ideation workshops"|
        dt7 == "Ideation workshops"|
        dt8 == "Ideation workshops"|
        dt9 == "Ideation workshops"|
        dt10 == "Ideation workshops"|
        dt11 == "Ideation workshops"|
        dt12 == "Ideation workshops"|
        dt13 == "Ideation workshops"|
        dt14 == "Ideation workshops"|  
        dt15 == "Ideation workshops"|
        dt16 == "Ideation workshops"|
        dt17 == "Ideation workshops" ~ 1,   
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    Content_writing = case_when(
      dt1 == "Copy or content writing"|
        dt2 == "Copy or content writing"|
        dt3 == "Copy or content writing"|
        dt4 == "Copy or content writing"|
        dt5 == "Copy or content writing"|
        dt6 == "Copy or content writing"|
        dt7 == "Copy or content writing"|
        dt8 == "Copy or content writing"|
        dt9 == "Copy or content writing"|
        dt10 == "Copy or content writing"|
        dt11 == "Copy or content writing"|
        dt12 == "Copy or content writing"|
        dt13 == "Copy or content writing"|
        dt14 == "Copy or content writing"|  
        dt15 == "Copy or content writing"|
        dt16 == "Copy or content writing"|
        dt17 == "Copy or content writing" ~ 1,   
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    Motion_design = case_when(
      dt1 == "Motion design"|
        dt2 == "Motion design"|
        dt3 == "Motion design"|
        dt4 == "Motion design"|
        dt5 == "Motion design"|
        dt6 == "Motion design"|
        dt7 == "Motion design"|
        dt8 == "Motion design"|
        dt9 == "Motion design"|
        dt10 == "Motion design"|
        dt11 == "Motion design"|
        dt12 == "Motion design"|
        dt13 == "Motion design"|
        dt14 == "Motion design"|  
        dt15 == "Motion design"|
        dt16 == "Motion design"|
        dt17 == "Motion design" ~ 1,   
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    Illustration_design = case_when(
      dt1 == "Illustration design"|
        dt2 == "Illustration design"|
        dt3 == "Illustration design"|
        dt4 == "Illustration design"|
        dt5 == "Illustration design"|
        dt6 == "Illustration design"|
        dt7 == "Illustration design"|
        dt8 == "Illustration design"|
        dt9 == "Illustration design"|
        dt10 == "Illustration design"|
        dt11 == "Illustration design"|
        dt12 == "Illustration design"|
        dt13 == "Illustration design"|
        dt14 == "Illustration design"|  
        dt15 == "Illustration design"|
        dt16 == "Illustration design"|
        dt17 == "Illustration design" ~ 1,   
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    Accessibility_review = case_when(
      dt1 == "Accessibility review"|
        dt2 == "Accessibility review"|
        dt3 == "Accessibility review"|
        dt4 == "Accessibility review"|
        dt5 == "Accessibility review"|
        dt6 == "Accessibility review"|
        dt7 == "Accessibility review"|
        dt8 == "Accessibility review"|
        dt9 == "Accessibility review"|
        dt10 == "Accessibility review"|
        dt11 == "Accessibility review"|
        dt12 == "Accessibility review"|
        dt13 == "Accessibility review"|
        dt14 == "Accessibility review"|  
        dt15 == "Accessibility review"|
        dt16 == "Accessibility review"|
        dt17 == "Accessibility review" ~ 1,   
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    User_flow_diagramming = case_when(
      dt1 == "User-flow diagramming"|
        dt2 == "User-flow diagramming"|
        dt3 == "User-flow diagramming"|
        dt4 == "User-flow diagramming"|
        dt5 == "User-flow diagramming"|
        dt6 == "User-flow diagramming"|
        dt7 == "User-flow diagramming"|
        dt8 == "User-flow diagramming"|
        dt9 == "User-flow diagramming"|
        dt10 == "User-flow diagramming"|
        dt11 == "User-flow diagramming"|
        dt12 == "User-flow diagramming"|
        dt13 == "User-flow diagramming"|
        dt14 == "User-flow diagramming"|  
        dt15 == "User-flow diagramming"|
        dt16 == "User-flow diagramming"|
        dt17 == "User-flow diagramming" ~ 1,   
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    Site_mapping = case_when(
      dt1 == "Site mapping/Information architecture"|
        dt2 == "Site mapping/Information architecture"|
        dt3 == "Site mapping/Information architecture"|
        dt4 == "Site mapping/Information architecture"|
        dt5 == "Site mapping/Information architecture"|
        dt6 == "Site mapping/Information architecture"|
        dt7 == "Site mapping/Information architecture"|
        dt8 == "Site mapping/Information architecture"|
        dt9 == "Site mapping/Information architecture"|
        dt10 == "Site mapping/Information architecture"|
        dt11 == "Site mapping/Information architecture"|
        dt12 == "Site mapping/Information architecture"|
        dt13 == "Site mapping/Information architecture"|
        dt14 == "Site mapping/Information architecture"|  
        dt15 == "Site mapping/Information architecture"|
        dt16 == "Site mapping/Information architecture"|
        dt17 == "Site mapping/Information architecture" ~ 1,   
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    Wireframing = case_when(
      dt1 == "Wireframing"|
        dt2 == "Wireframing"|
        dt3 == "Wireframing"|
        dt4 == "Wireframing"|
        dt5 == "Wireframing"|
        dt6 == "Wireframing"|
        dt7 == "Wireframing"|
        dt8 == "Wireframing"|
        dt9 == "Wireframing"|
        dt10 == "Wireframing"|
        dt11 == "Wireframing"|
        dt12 == "Wireframing"|
        dt13 == "Wireframing"|
        dt14 == "Wireframing"|  
        dt15 == "Wireframing"|
        dt16 == "Wireframing"|
        dt17 == "Wireframing" ~ 1,   
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    prototyping = case_when(
      dt1 == "Low fidelity prototyping"|
        dt2 == "Low fidelity prototyping"|
        dt3 == "Low fidelity prototyping"|
        dt4 == "Low fidelity prototyping"|
        dt5 == "Low fidelity prototyping"|
        dt6 == "Low fidelity prototyping"|
        dt7 == "Low fidelity prototyping"|
        dt8 == "Low fidelity prototyping"|
        dt9 == "Low fidelity prototyping"|
        dt10 == "Low fidelity prototyping"|
        dt11 == "Low fidelity prototyping"|
        dt12 == "Low fidelity prototyping"|
        dt13 == "Low fidelity prototyping"|
        dt14 == "Low fidelity prototyping"|  
        dt15 == "Low fidelity prototyping"|
        dt16 == "Low fidelity prototyping"|
        dt17 == "Low fidelity prototyping" ~ 1,   
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    High_prototyping = case_when(
      dt1 == "High fidelity prototyping"|
        dt2 == "High fidelity prototyping"|
        dt3 == "High fidelity prototyping"|
        dt4 == "High fidelity prototyping"|
        dt5 == "High fidelity prototyping"|
        dt6 == "High fidelity prototyping"|
        dt7 == "High fidelity prototyping"|
        dt8 == "High fidelity prototyping"|
        dt9 == "High fidelity prototyping"|
        dt10 == "High fidelity prototyping"|
        dt11 == "High fidelity prototyping"|
        dt12 == "High fidelity prototyping"|
        dt13 == "High fidelity prototyping"|
        dt14 == "High fidelity prototyping"|  
        dt15 == "High fidelity prototyping"|
        dt16 == "High fidelity prototyping"|
        dt17 == "High fidelity prototyping" ~ 1,   
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    Code_level_prototyping = case_when(
      dt1 == "Code level prototyping"|
        dt2 == "Code level prototyping"|
        dt3 == "Code level prototyping"|
        dt4 == "Code level prototyping"|
        dt5 == "Code level prototyping"|
        dt6 == "Code level prototyping"|
        dt7 == "Code level prototyping"|
        dt8 == "Code level prototyping"|
        dt9 == "Code level prototyping"|
        dt10 == "Code level prototyping"|
        dt11 == "Code level prototyping"|
        dt12 == "Code level prototyping"|
        dt13 == "Code level prototyping"|
        dt14 == "Code level prototyping"|  
        dt15 == "Code level prototyping"|
        dt16 == "Code level prototyping"|
        dt17 == "Code level prototyping" ~ 1,   
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    Service_simulations = case_when(
      dt1 == "Service simulations"|
        dt2 == "Service simulations"|
        dt3 == "Service simulations"|
        dt4 == "Service simulations"|
        dt5 == "Service simulations"|
        dt6 == "Service simulations"|
        dt7 == "Service simulations"|
        dt8 == "Service simulations"|
        dt9 == "Service simulations"|
        dt10 == "Service simulations"|
        dt11 == "Service simulations"|
        dt12 == "Service simulations"|
        dt13 == "Service simulations"|
        dt14 == "Service simulations"|  
        dt15 == "Service simulations"|
        dt16 == "Service simulations"|
        dt17 == "Service simulations" ~ 1,   
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    Feasibility_review = case_when(
      dt1 == "Feasibility review with stakeholders"|
        dt2 == "Feasibility review with stakeholders"|
        dt3 == "Feasibility review with stakeholders"|
        dt4 == "Feasibility review with stakeholders"|
        dt5 == "Feasibility review with stakeholders"|
        dt6 == "Feasibility review with stakeholders"|
        dt7 == "Feasibility review with stakeholders"|
        dt8 == "Feasibility review with stakeholders"|
        dt9 == "Feasibility review with stakeholders"|
        dt10 == "Feasibility review with stakeholders"|
        dt11 == "Feasibility review with stakeholders"|
        dt12 == "Feasibility review with stakeholders"|
        dt13 == "Feasibility review with stakeholders"|
        dt14 == "Feasibility review with stakeholders"|  
        dt15 == "Feasibility review with stakeholders"|
        dt16 == "Feasibility review with stakeholders"|
        dt17 == "Feasibility review with stakeholders" ~ 1,   
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    Customer_creation = case_when(
      dt1 == "Co-creation with customers"|
        dt2 == "Co-creation with customers"|
        dt3 == "Co-creation with customers"|
        dt4 == "Co-creation with customers"|
        dt5 == "Co-creation with customers"|
        dt6 == "Co-creation with customers"|
        dt7 == "Co-creation with customers"|
        dt8 == "Co-creation with customers"|
        dt9 == "Co-creation with customers"|
        dt10 == "Co-creation with customers"|
        dt11 == "Co-creation with customers"|
        dt12 == "Co-creation with customers"|
        dt13 == "Co-creation with customers"|
        dt14 == "Co-creation with customers"|  
        dt15 == "Co-creation with customers"|
        dt16 == "Co-creation with customers"|
        dt17 == "Co-creation with customers" ~ 1,   
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    Design_sprint = case_when(
      dt1 == "Design sprint (Design cooker)"|
        dt2 == "Design sprint (Design cooker)"|
        dt3 == "Design sprint (Design cooker)"|
        dt4 == "Design sprint (Design cooker)"|
        dt5 == "Design sprint (Design cooker)"|
        dt6 == "Design sprint (Design cooker)"|
        dt7 == "Design sprint (Design cooker)"|
        dt8 == "Design sprint (Design cooker)"|
        dt9 == "Design sprint (Design cooker)"|
        dt10 == "Design sprint (Design cooker)"|
        dt11 == "Design sprint (Design cooker)"|
        dt12 == "Design sprint (Design cooker)"|
        dt13 == "Design sprint (Design cooker)"|
        dt14 == "Design sprint (Design cooker)"|  
        dt15 == "Design sprint (Design cooker)"|
        dt16 == "Design sprint (Design cooker)"|
        dt17 == "Design sprint (Design cooker)" ~ 1,   
    ))


DMA1 <- DMA1 %>%
  dplyr::rename(
    metrics=
   `To what extent do you use metrics to measure the quality of your designs?\r\n\r\nPopular UX metrics include user satisfaction ratings, task completion rates, returning visitors, time on task, conversion...`
  )

DMA19 <- DMA1 %>%
  dplyr::select(contains("use metrics"))

DMA1$metrics2 <- DMA19[,1]
  
DMA1 <- DMA1 %>%  
  dplyr::mutate(Metrics_usage = case_when(
      metrics == "I never measure metrics for any project I work on"  ~ 1,
      metrics2 == "I don't collect or define metrics for any project" ~ 1,
      metrics == "I often measure and monitor metrics for the projects I work on"  ~ 4,
      metrics == "I often"  ~ 4,
      metrics == "I rarely measure metrics for the projects I work on"  ~ 2,
      metrics2 == "I collect or receive at least a few metrics but not for every project"  ~ 3,
      metrics2 == "I consistently collect or receive metrics for most or all projects I work on"  ~ 4,
      metrics2 == "I consistently collect or receive metrics for most or all projects I work on and connect these metrics with user and/or business value"  ~ 5,
      metrics == "I’m not sure" ~ 1,
      metrics2 == "I’m not sure" ~ 1,
      metrics == "N/A " ~ 1,
      metrics == "I always measure  and monitor metrics for the projects I work on, and monitor the customer/business impact post-launch" ~ 5,
      metrics == "I sometimes measure metrics for the projects I work on"  ~ 3,
      startsWith(as.character(metrics), "I often") ~ 4,
      startsWith(as.character(metrics), "I never") ~ 1,
      startsWith(as.character(metrics), "I always") ~ 5,
      startsWith(as.character(metrics), "N/A") ~ 1
    )
  )

DMA1 <- DMA1 %>%  
  dplyr::rename(processes=
                  `In general, how consistent are the UX processes & techniques across your local Design team? `) 
DMA1 <- DMA1 %>%  
  dplyr::mutate(processes = as.character(processes)) %>%
  dplyr::mutate(processes_consistency = case_when(
    processes == "Very inconsistent. There are very few established processes and techniques, but are not used frequently"~ 1,
    processes == "Inconsistent. There are very few established processes and techniques, but are not used frequently"~ 2,
    processes == "Somewhat consistent. Many processes and techniques are established but are not used frequently"~ 3,
    processes == "Consistent. Many process & techniques are established and are similar across teams and projects"~ 4,
    processes == "Very consistent. There are systematic, established frameworks across the organization, which are shared, maintained, and improved" ~ 5,
    startsWith(as.character(processes), "Somewhat") ~ 3,
    startsWith(as.character(processes), "Very inconsistent") ~ 1,
    startsWith(as.character(processes), "Fully inconsistent") ~ 1,
    startsWith(as.character(processes), "Inconsistent") ~ 2,
    startsWith(as.character(processes), "Consistent") ~ 4,
    startsWith(as.character(processes), "Very consistent") ~ 5,
    startsWith(as.character(processes), "Fully consistent") ~ 5
    )
  )


####################### just need to add sums etc
DMA6 <- DMA1 %>%
  dplyr::select(contains("Which non-Design Team stakeholders do you consistently"))
colnames(DMA6)[1] <- "stakeholders"
DMA1$stakeholders <- DMA6$stakeholders
  
DMA1 <- DMA1 %>%  
  tidyr::separate(stakeholders,
    c("nd1", "nd2", "nd3", "nd4", "nd5", "nd6", "nd7", "nd8", "nd9", "nd10",
      "nd11", "nd12", "nd13"),
    sep = ";"
  ) %>%
  rowwise() %>%
  mutate(na_count = sum(is.na(c_across(all_of(c("nd1", "nd2", "nd3", "nd4", "nd5", "nd6", "nd7", "nd8", "nd9", "nd10",
      "nd11", "nd12", "nd13")))))) %>%
  dplyr::mutate(nd_stakeholders_points = 13 - na_count)


DMA1 <- DMA1 %>%
  dplyr::mutate(  
    stakeholders_Accessibility_team = case_when(
      nd1 == "Accessibility team"|
        nd2 == "Accessibility team"|
        nd3 == "Accessibility team"|
        nd4 == "Accessibility team"|
        nd5 == "Accessibility team"|
        nd6 == "Accessibility team"|
        nd7 == "Accessibility team"|
        nd8 == "Accessibility team"|
        nd9 == "Accessibility team"|
        nd10 == "Accessibility team"|
        nd11 == "Accessibility team"|
        nd12 == "Accessibility team"|
        nd13 == "Accessibility team" ~ 1,   
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    stakeholders_marketing = case_when(
      nd1 == "Branding and Marketing"|
        nd2 == "Branding and Marketing"|
        nd3 == "Branding and Marketing"|
        nd4 == "Branding and Marketing"|
        nd5 == "Branding and Marketing"|
        nd6 == "Branding and Marketing"|
        nd7 == "Branding and Marketing"|
        nd8 == "Branding and Marketing"|
        nd9 == "Branding and Marketing"|
        nd10 == "Branding and Marketing"|
        nd11 == "Branding and Marketing"|
        nd12 == "Branding and Marketing"|
        nd13 == "Branding and Marketing" ~ 1,   
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    stakeholders_coaches = case_when(
      nd1 == "Coaches (PACE, AGILE)"|
        nd2 == "Coaches (PACE, AGILE)"|
        nd3 == "Coaches (PACE, AGILE)"|
        nd4 == "Coaches (PACE, AGILE)"|
        nd5 == "Coaches (PACE, AGILE)"|
        nd6 == "Coaches (PACE, AGILE)"|
        nd7 == "Coaches (PACE, AGILE)"|
        nd8 == "Coaches (PACE, AGILE)"|
        nd9 == "Coaches (PACE, AGILE)"|
        nd10 == "Coaches (PACE, AGILE)"|
        nd11 == "Coaches (PACE, AGILE)"|
        nd12 == "Coaches (PACE, AGILE)"|
        nd13 == "Coaches (PACE, AGILE)" ~ 1,   
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    stakeholders_CJE = case_when(
      nd1 == "Customer Journey Experts (CJE)"|
        nd2 == "Customer Journey Experts (CJE)"|
        nd3 == "Customer Journey Experts (CJE)"|
        nd4 == "Customer Journey Experts (CJE)"|
        nd5 == "Customer Journey Experts (CJE)"|
        nd6 == "Customer Journey Experts (CJE)"|
        nd7 == "Customer Journey Experts (CJE)"|
        nd8 == "Customer Journey Experts (CJE)"|
        nd9 == "Customer Journey Experts (CJE)"|
        nd10 == "Customer Journey Experts (CJE)"|
        nd11 == "Customer Journey Experts (CJE)"|
        nd12 == "Customer Journey Experts (CJE)"|
        nd13 == "Customer Journey Experts (CJE)" ~ 1,   
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    stakeholders_CX = case_when(
      nd1 == "Customer Experience team (CX)"|
        nd2 == "Customer Experience team (CX)"|
        nd3 == "Customer Experience team (CX)"|
        nd4 == "Customer Experience team (CX)"|
        nd5 == "Customer Experience team (CX)"|
        nd6 == "Customer Experience team (CX)"|
        nd7 == "Customer Experience team (CX)"|
        nd8 == "Customer Experience team (CX)"|
        nd9 == "Customer Experience team (CX)"|
        nd10 == "Customer Experience team (CX)"|
        nd11 == "Customer Experience team (CX)"|
        nd12 == "Customer Experience team (CX)"|
        nd13 == "Customer Experience team (CX)" ~ 1,   
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    stakeholders_Contact_center = case_when(
      nd1 == "Customer Support Team (Contact Centre)"|
        nd2 == "Customer Support Team (Contact Centre)"|
        nd3 == "Customer Support Team (Contact Centre)"|
        nd4 == "Customer Support Team (Contact Centre)"|
        nd5 == "Customer Support Team (Contact Centre)"|
        nd6 == "Customer Support Team (Contact Centre)"|
        nd7 == "Customer Support Team (Contact Centre)"|
        nd8 == "Customer Support Team (Contact Centre)"|
        nd9 == "Customer Support Team (Contact Centre)"|
        nd10 == "Customer Support Team (Contact Centre)"|
        nd11 == "Customer Support Team (Contact Centre)"|
        nd12 == "Customer Support Team (Contact Centre)"|
        nd13 == "Customer Support Team (Contact Centre)" ~ 1,   
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    stakeholders_legal = case_when(
      nd1 == "legal"|
        nd2 == "legal"|
        nd3 == "legal"|
        nd4 == "legal"|
        nd5 == "legal"|
        nd6 == "legal"|
        nd7 == "legal"|
        nd8 == "legal"|
        nd9 == "legal"|
        nd10 == "legal"|
        nd11 == "legal"|
        nd12 == "legal"|
        nd13 == "legal" ~ 1,   
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    stakeholders_PO = case_when(
      nd1 == "Product owners"|
        nd2 == "Product owners"|
        nd3 == "Product owners"|
        nd4 == "Product owners"|
        nd5 == "Product owners"|
        nd6 == "Product owners"|
        nd7 == "Product owners"|
        nd8 == "Product owners"|
        nd9 == "Product owners"|
        nd10 == "Product owners"|
        nd11 == "Product owners"|
        nd12 == "Product owners"|
        nd13 == "Product owners" ~ 1,   
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    stakeholders_risk = case_when(
      nd1 == "Risk and Compliance"|
        nd2 == "Risk and Compliance"|
        nd3 == "Risk and Compliance"|
        nd4 == "Risk and Compliance"|
        nd5 == "Risk and Compliance"|
        nd6 == "Risk and Compliance"|
        nd7 == "Risk and Compliance"|
        nd8 == "Risk and Compliance"|
        nd9 == "Risk and Compliance"|
        nd10 == "Risk and Compliance"|
        nd11 == "Risk and Compliance"|
        nd12 == "Risk and Compliance"|
        nd13 == "Risk and Compliance" ~ 1,   
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    stakeholders_Senior_management = case_when(
      nd1 == "Senior management"|
        nd2 == "Senior management"|
        nd3 == "Senior management"|
        nd4 == "Senior management"|
        nd5 == "Senior management"|
        nd6 == "Senior management"|
        nd7 == "Senior management"|
        nd8 == "Senior management"|
        nd9 == "Senior management"|
        nd10 == "Senior management"|
        nd11 == "Senior management"|
        nd12 == "Senior management"|
        nd13 == "Senior management" ~ 1,   
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    stakeholders_IT = case_when(
      nd1 == "The IT/Development team relevant to each project"|
        nd2 == "The IT/Development team relevant to each project"|
        nd3 == "The IT/Development team relevant to each project"|
        nd4 == "The IT/Development team relevant to each project"|
        nd5 == "The IT/Development team relevant to each project"|
        nd6 == "The IT/Development team relevant to each project"|
        nd7 == "The IT/Development team relevant to each project"|
        nd8 == "The IT/Development team relevant to each project"|
        nd9 == "The IT/Development team relevant to each project"|
        nd10 == "The IT/Development team relevant to each project"|
        nd11 == "The IT/Development team relevant to each project"|
        nd12 == "The IT/Development team relevant to each project"|
        nd13 == "The IT/Development team relevant to each project" ~ 1,   
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    stakeholders_Third_parties = case_when(
      nd1 == "Third party vendors"|
        nd2 == "Third party vendors"|
        nd3 == "Third party vendors"|
        nd4 == "Third party vendors"|
        nd5 == "Third party vendors"|
        nd6 == "Third party vendors"|
        nd7 == "Third party vendors"|
        nd8 == "Third party vendors"|
        nd9 == "Third party vendors"|
        nd10 == "Third party vendors"|
        nd11 == "Third party vendors"|
        nd12 == "Third party vendors"|
        nd13 == "Third party vendors" ~ 1,   
    ))

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    stakeholders_Development = case_when(
      nd1 == "Development"|
        nd2 == "Development"|
        nd3 == "Development"|
        nd4 == "Development"|
        nd5 == "Development"|
        nd6 == "Development"|
        nd7 == "Development"|
        nd8 == "Development"|
        nd9 == "Development"|
        nd10 == "Development"|
        nd11 == "Development"|
        nd12 == "Development"|
        nd13 == "Development" ~ 1,   
    ))


###########################################################

  
#### Let op, de laagste antwoorden kwamen nog niet voor!
DMA1 <- DMA1 %>%
    dplyr::mutate(collaboration = case_when(
      `How do non-Design Team stakeholders collaborate with your Design Team?`==
        "Some accept UX and collaborate with the Design Team" ~ 3,
      `How do non-Design Team stakeholders collaborate with your Design Team?`==
        "Some accept UX and collaborate with the Design Team" ~ 3,
      `How do non-Design Team stakeholders collaborate with your Design Team?`==
        "Most accept UX and collaborate with the Design Team"  ~ 4,
      `How do non-Design Team stakeholders collaborate with your Design Team?`==
        "All strongly accept UX and collaborate with the Design Team"  ~ 5,
      `How do non-Design Team stakeholders collaborate with your Design Team?`==
        "All strongly respect UX and intensively collaborate with the Design Team"  ~ 5,
      `How do non-Design Team stakeholders collaborate with your Design Team?`==
        "I'm not sure"  ~ 1)
    )


###### not all answers included it seems
DMA1 <- DMA1 %>%
  dplyr::mutate(improvement = case_when(
    `Does your Design team improve on Design processes & techniques?`==
      "Creating design deliverables is our only focus, we don't have time to improve on our design process. " ~1,
    `Does your Design team improve on Design processes & techniques?`==
      "We sometimes have time to improve our design process. But when we do, it's on an ad-hoc basis" ~ 2,
    `Does your Design team improve on Design processes & techniques?`==
      "We make time to improve our design process, we have a set cadence in place where we review them" ~ 3,
    `Does your Design team improve on Design processes & techniques?`==
      "I'm not sure" ~ 1,
    `Does your Design team improve on your Design processes & techniques?`==
      "We don't have time to improve on our Design processes and techniques, creating UX deliverables is our only focus" ~1,
    `Does your Design team improve on your Design processes & techniques?`==
      "We sometimes have time to improve our Design processes and techniques" ~2,
    `Does your Design team improve on your Design processes & techniques?`==
      "We always have time to improve our Design processes and techniques" ~3,
    `Does your Design team improve on your Design processes & techniques?`==
      "I'm not sure" ~1,
    TRUE ~ 1
     )
  )



##################
DMA5 <- DMA1 %>%
  dplyr::select(contains("outside"))
colnames(DMA5)[1] <- "activities"
DMA1$activities <- DMA5$activities

DMA1 <- DMA1 %>%  
  tidyr::separate(activities,
                  c("act1", "act2", "act3", "act4", "act5"),
                  sep = ";"
  ) %>%
  rowwise() %>%
  mutate(na_count = sum(is.na(c_across(all_of(c("act1", "act2", "act3", "act4", "act5")))))) %>%
  dplyr::mutate(activities_point = 5 - na_count)


DMA1 <- DMA1 %>%
  dplyr::mutate(  
    activities_squad = case_when(
      act1 == "Squad level activities such as backlog refinement, retrospectives, and stand-ups "|
        act2 == "Squad level activities such as backlog refinement, retrospectives, and stand-ups "|
        act3 == "Squad level activities such as backlog refinement, retrospectives, and stand-ups "|
        act4 == "Squad level activities such as backlog refinement, retrospectives, and stand-ups "|
        act5 == "Squad level activities such as backlog refinement, retrospectives, and stand-ups " ~ 1,   
    )) %>%
  dplyr::mutate(
    activities_squad = as.numeric(activities_squad)
  ) %>%
  dplyr::mutate(
    activities_squad = case_when(
      is.na(activities_squad) ~ 0,
      !is.na(activities_squad) ~1)
  )

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    activities_tribe = case_when(
      act1 == "Tribe level activities such as epic commitment sessions and objective and key results (OKR) setting"|
        act2 == "Tribe level activities such as epic commitment sessions and objective and key results (OKR) setting"|
        act3 == "Tribe level activities such as epic commitment sessions and objective and key results (OKR) setting"|
        act4 == "Tribe level activities such as epic commitment sessions and objective and key results (OKR) setting"|
        act5 == "Tribe level activities such as epic commitment sessions and objective and key results (OKR) setting" ~ 1,   
    )) %>%
  dplyr::mutate(
    activities_tribe = as.numeric(activities_tribe)
  ) %>%
  dplyr::mutate(
    activities_tribe = case_when(
      is.na(activities_tribe) ~ 0,
      !is.na(activities_tribe) ~1)
  )

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    activities_Q = case_when(
      act1 == "Quarterly business review (QBR-Refinement of quarterly priorities) with top management, tribe leads, or chapter leads"|
        act2 == "Quarterly business review (QBR-Refinement of quarterly priorities) with top management, tribe leads, or chapter leads"|
        act3 == "Quarterly business review (QBR-Refinement of quarterly priorities) with top management, tribe leads, or chapter leads"|
        act4 == "Quarterly business review (QBR-Refinement of quarterly priorities) with top management, tribe leads, or chapter leads"|
        act5 == "Quarterly business review (QBR-Refinement of quarterly priorities) with top management, tribe leads, or chapter leads" ~ 1,   
    )) %>%
  dplyr::mutate(
    activities_Q = as.numeric(activities_Q)
  ) %>%
  dplyr::mutate(
    activities_Q = case_when(
      is.na(activities_Q) ~ 0,
      !is.na(activities_Q) ~1)
  )

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    activities_non = case_when(
      act1 == "None of the above"|
        act2 == "None of the above"|
        act3 == "None of the above"|
        act4 == "None of the above"|
        act5 == "None of the above" ~ 1,   
    )) %>%
  dplyr::mutate(
    activities_non = as.numeric(activities_non)
  ) %>%
  dplyr::mutate(
    activities_non = case_when(
      is.na(activities_non) ~ 0,
      !is.na(activities_non) ~1)
  )

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    activities_not_sure = case_when(
      act1 == "I'm not sure"|
        act2 == "I'm not sure"|
        act3 == "I'm not sure"|
        act4 == "I'm not sure"|
        act5 == "I'm not sure" ~ 1,   
    )) %>% 
  dplyr::mutate(
    activities_not_sure = as.numeric(activities_not_sure)
  ) %>%
  dplyr::mutate(
    activities_not_sure = case_when(
      is.na(activities_not_sure) ~ 0,
      !is.na(activities_not_sure) ~1)
  )
  



DMA1 <- DMA1 %>%
  dplyr::mutate(
    activities_pointers = activities_squad + activities_tribe + activities_Q
  )

DMA1 <- DMA1 %>%
  dplyr::mutate(
    customer_focus = case_when(
      `Are customer needs considered when setting project goals?`== "I'm not sure" ~ 1,
      `Are customer needs considered when setting project goals?`== "Never, Customer needs are considered" ~ 1,
      `Are customer needs considered when setting project goals?`== "Sometimes, Customer needs are considered" ~ 2,
      `Are customer needs considered when setting project goals?`== "Always, Customer needs are clear before setting project goals" ~ 3,
      startsWith(as.character(`Are customer needs considered when setting project goals?`), "I'm not sure") ~ 1,
      startsWith(as.character(`Do business/ project teams set their business goals i.e. OKR's or KPI's, based on customer needs?`), "I'm not sure") ~ 1,
      startsWith(as.character(`Do business/ project teams set their business goals i.e. OKR's or KPI's, based on customer needs?`), "No, business goals are based on business needs") ~ 1,
      startsWith(as.character(`Do business/ project teams set their business goals i.e. OKR's or KPI's, based on customer needs?`), "Yes, but the customer needs are mostly assumed") ~ 2,
      startsWith(as.character(`Do business/ project teams set their business goals i.e. OKR's or KPI's, based on customer needs?`), "Yes, and the customer needs are grounded in research") ~ 3
    )
  )

DMA7 <- DMA1 %>%
  dplyr::select(contains("to what degree do User research insights impact business priorities and direction?"))
colnames(DMA7)[1] <- "user_research_insights"
DMA1$user_research_insights <- DMA7$user_research_insights

DMA1 <- DMA1 %>%
  dplyr::mutate(
    user_research_insightss = case_when(
      user_research_insights ==
        "Always, User research insights significantly influence priorities, often driving the decision-making process" ~ 5,
      user_research_insights ==
        "Often, User research insights are one of the most important sources to set priorities" ~ 4,
      user_research_insights ==
        "Sometimes, User research insights play a role in setting project goals, but isn't a high priority" ~ 3,
      user_research_insights =="I'm not sure" ~ 1,
      user_research_insights =="Rarely, User research insights have little impact in the setting of project goals " ~ 2,
      startsWith(as.character(user_research_insights), "Rarely, User research insights have little impact in the setting of project goals") ~ 2,
      startsWith(as.character(user_research_insights), "Always") ~ 5,
      `To what degree do user research insights impact ING's business leads (tribe and product area leads) when they set company-wide priorities?` ==
        "It does not have a lot of impact, outside of some exceptions" ~1,
      `To what degree do user research insights impact ING's business leads (tribe and product area leads) when they set company-wide priorities?` ==
        "User research insights have no impact in the setting of company wide priorities" ~2,
      `To what degree do user research insights impact ING's business leads (tribe and product area leads) when they set company-wide priorities?` ==
        "It gets discussed and plays a role, but isn't high in priority" ~3,
      `To what degree do user research insights impact ING's business leads (tribe and product area leads) when they set company-wide priorities?` ==
        "It is one of the most important sources to set priorities" ~4,
      `To what degree do user research insights impact ING's business leads (tribe and product area leads) when they set company-wide priorities?` ==
        "I'm not sure" ~1,
      )
  )

DMA1 <- DMA1 %>%
  dplyr::mutate(financial_resources= case_when(
    `What financial resources does the Design Team have for UX work?` == "I'm not sure" ~ 1,
    `What financial resources does the Design Team have for UX work?` == "There is no money spent on UX work" ~ 1,
    `What financial resources does the Design Team have for UX work?` == "There is some money spent, but there is no dedicated UX budget" ~ 2,
    `What financial resources does the Design Team have for UX work?` == "There is dedicated UX budget, but it's not enough" ~ 3,
    `What financial resources does the Design Team have for UX work?` == "There is enough UX budget" ~ 4,
    startsWith(as.character(`What financial resources does the Design Team have for UX work?`), "I") ~ 1,
    TRUE ~1
  ))

DMA1 <- DMA1 %>%
  dplyr::mutate(leadership_support= case_when(
    `How does ING's leadership (Tribe leads and above) support UX? ` == "I'm not sure" ~ 1,
    `How does ING's leadership (Tribe leads and above) support UX? ` == "Tribe leads are aware of UX but indifferent to it" ~ 2,
    `How does ING's leadership (Tribe leads and above) support UX? ` == "Tribe leads do not support UX at all " ~ 1,
    `How does ING's leadership (Tribe leads and above) support UX? ` == "Tribe leads accept the need for UX but some aren't convinced" ~ 3,
    `How does ING's leadership (Tribe leads and above) support UX? ` == "All Tribe leads and above support UX, even at the highest level" ~ 5,
    `How does ING's leadership (Tribe leads and above) support UX? ` == "Most Tribe leads strongly support UX" ~ 4,
    #startsWith(as.character(`How does ING's leadership (Tribe leads and above) support UX? `), "I") ~ 1
  ))
      

DMA7 <- DMA1 %>%
  dplyr::select(contains("to what degree do User research insights impact business priorities and direction?"))
colnames(DMA7)[1] <- "user_research_insights"
DMA1$user_research_insights <- DMA7$user_research_insights


DMA8 <- DMA1 %>%
  dplyr::select(contains("how would you assess the alignment of Design"))
colnames(DMA8)[1] <- "alignment_it_business"
DMA1$alignment_it_business <- DMA8$alignment_it_business 

DMA20 <- DMA1 %>%
  dplyr::select(contains("how would you assess their alignment with Design"))
colnames(DMA20)[1] <- "alignment_it_business2"
DMA1$alignment_it_business2 <- as.character(DMA20$alignment_it_business2)  

DMA1 <- DMA1 %>%
  dplyr::mutate(it_business_alignment = case_when(
    alignment_it_business == "Business, Design and IT are aligned" ~ 6,
    alignment_it_business == "Design leads. Business and IT follow" ~ 5,
    alignment_it_business == "Business and Design are aligned. IT follows" ~ 4,
    alignment_it_business == "Business leads. Design and IT follow" ~ 3,
    alignment_it_business == "Business and IT are aligned. Design follows" ~ 2,
    alignment_it_business == "IT leads. Business and Design follow" ~ 1,
    alignment_it_business2 == "Business, Design and IT are aligned" ~ 6,
    alignment_it_business2 == "Design leads. Business and IT follow" ~ 5,
    alignment_it_business2 == "Business and Design are aligned. IT follows" ~ 4,
    alignment_it_business2 == "Business leads. Design and IT follow" ~ 3,
    alignment_it_business2 == "Business and IT are aligned. Design follows" ~ 2,
    alignment_it_business2 == "IT leads. Business and Design follow" ~ 1,
    TRUE ~ 1)
    )


DMA9 <- DMA1 %>%
  dplyr::select(contains("to push for the 'best-in-class' User Experience"))
colnames(DMA9)[1] <- "opportunity_innovation_UX"
DMA1$opportunity_innovation_UX <- DMA9$opportunity_innovation_UX 

DMA10 <- DMA1 %>%
  dplyr::select(contains("and push for the 'best-in-class' customer experience"))
colnames(DMA10)[1] <- "opportunity_innovation_cust"
DMA1$opportunity_innovation_cust <- DMA10$opportunity_innovation_cust


DMA1 <- DMA1 %>%
  dplyr::mutate(innovation_opportunity = case_when(
    opportunity_innovation_UX =="Never" ~ 1,
    opportunity_innovation_cust =="Never" ~ 1,
    opportunity_innovation_UX =="Rarely" ~ 2,
    opportunity_innovation_cust =="Rarely" ~ 2,
    opportunity_innovation_UX =="Sometimes" ~ 3,
    opportunity_innovation_cust =="Sometimes" ~ 3,
    opportunity_innovation_UX =="Often" ~ 4,
    opportunity_innovation_cust =="Often" ~ 4,
    opportunity_innovation_UX =="Always" ~ 5,
    opportunity_innovation_cust =="Always" ~ 5
  ))

DMA11 <- DMA1 %>%
  dplyr::select(contains("evangelises"))
colnames(DMA11)[2] <- "cust_centre"
colnames(DMA11)[1] <- "ux_centre"
DMA1$cust_centre <- DMA11$cust_centre
DMA1$ux_centre <- DMA11$ux_centre

DMA1 <- DMA1 %>%
  tidyr::separate(cust_centre,
                  c("ev1", "ev2", "ev3", "ev4", "ev5", "ev6", "ev7"),
                  sep = ";"
  ) %>%
    rowwise() %>%
    mutate(na_count = sum(is.na(c_across(all_of(c("ev1", "ev2", "ev3", "ev4", "ev5", "ev6", "ev7")))))) %>%
    dplyr::mutate(eva_point = 7 - na_count)

DMA1 <- DMA1 %>%
  tidyr::separate(ux_centre,
                  c("evu1", "evu2", "evu3", "evu4", "evu5", "evu6", "evu7"),
                  sep = ";"
  ) %>%
  rowwise() %>%
  mutate(na_count = sum(is.na(c_across(all_of(c("evu1", "evu2", "evu3", "evu4", "evu5", "evu6", "evu7")))))) %>%
  dplyr::mutate(evau_point = 7 - na_count)


DMA1 <- DMA1 %>%
  dplyr::mutate(  
    channel_not_sure = case_when(
      ev1 == "I'm not sure"|
        ev2 == "I'm not sure"|
        ev3 == "I'm not sure"|
        ev4 == "I'm not sure"|
        ev5 == "I'm not sure"|
        ev6 == "I'm not sure"|
        ev7 == "I'm not sure"|
        evu1 == "I'm not suree"|
        evu2 == "I'm not sure"|
        evu3 == "I'm not sure"|
        evu4 == "I'm not sure"|
        evu5 == "I'm not sure"|
        evu6 == "I'm not sure"|
        evu7 == "I'm not sure"~ 1,   
    )) %>%
  dplyr::mutate(
    channel_not_sure = as.numeric(channel_not_sure)
  ) %>%
  dplyr::mutate(
    channel_not_sure = case_when(
      is.na(channel_not_sure) ~ 0,
      !is.na(channel_not_sure) ~1)
  )

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    channel_dont = case_when(
      ev1 == "We don't"|
        ev2 == "We don't"|
        ev3 == "We don't"|
        ev4 == "We don't"|
        ev5 == "We don't"|
        ev6 == "We don't"|
        ev7 == "We don't"|
        evu1 == "We don't"|
        evu2 == "We don't"|
        evu3 == "We don't"|
        evu4 == "We don't"|
        evu5 == "We don't"|
        evu6 == "We don't"|
        evu7 == "We don't"~ 1,   
    )) %>%
  dplyr::mutate(
    channel_dont = as.numeric(channel_dont)
  ) %>%
  dplyr::mutate(
    channel_dont = case_when(
      is.na(channel_dont) ~ 0,
      !is.na(channel_dont) ~1)
  )

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    channel_team = case_when(
      ev1 == "Within our Design Team"|
        ev2 == "Within our Design Team"|
        ev3 == "Within our Design Team"|
        ev4 == "Within our Design Team"|
        ev5 == "Within our Design Team"|
        ev6 == "Within our Design Team"|
        ev7 == "Within our Design Team"|
        evu1 == "Within our Design Team"|
        evu2 == "Within our Design Team"|
        evu3 == "Within our Design Team"|
        evu4 == "Within our Design Team"|
        evu5 == "Within our Design Team"|
        evu6 == "Within our Design Team"|
        evu7 == "Within our Design Team"~ 1,   
    )) %>%
  dplyr::mutate(
    channel_team = as.numeric(channel_team)
  ) %>%
  dplyr::mutate(
    channel_team = case_when(
      is.na(channel_team) ~ 0,
      !is.na(channel_team) ~1)
  )

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    channel_tribe = case_when(
      ev1 == "Within our Tribe"|
        ev2 == "Within our Tribe"|
        ev3 == "Within our Tribe"|
        ev4 == "Within our Tribe"|
        ev5 == "Within our Tribe"|
        ev6 == "Within our Tribe"|
        ev7 == "Within our Tribe"|
        evu1 == "Within our Tribe"|
        evu2 == "Within our Tribe"|
        evu3 == "Within our Tribe"|
        evu4 == "Within our Tribe"|
        evu5 == "Within our Tribe"|
        evu6 == "Within our Tribe"|
        evu7 == "Within our Tribe"~ 1,   
    )) %>%
  dplyr::mutate(
    channel_tribe = as.numeric(channel_tribe)
  ) %>%
  dplyr::mutate(
    channel_tribe = case_when(
      is.na(channel_tribe) ~ 0,
      !is.na(channel_tribe) ~1)
  )

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    channel_across_tribes = case_when(
      ev1 == "Across other tribes outside of our own"|
        ev2 == "Across other tribes outside of our own"|
        ev3 == "Across other tribes outside of our own"|
        ev4 == "Across other tribes outside of our own"|
        ev5 == "Across other tribes outside of our own"|
        ev6 == "Across other tribes outside of our own"|
        ev7 == "Across other tribes outside of our own"|
        evu1 == "Across other tribes outside of our own"|
        evu2 == "Across other tribes outside of our own"|
        evu3 == "Across other tribes outside of our own"|
        evu4 == "Across other tribes outside of our own"|
        evu5 == "Across other tribes outside of our own"|
        evu6 == "Across other tribes outside of our own"|
        evu7 == "Across other tribes outside of our own"~ 1,   
    )) %>%
  dplyr::mutate(
    channel_across_tribes = as.numeric(channel_across_tribes)
  ) %>%
  dplyr::mutate(
    channel_across_tribes = case_when(
      is.na(channel_across_tribes) ~ 0,
      !is.na(channel_across_tribes) ~1)
  )

DMA1 <- DMA1 %>%
  dplyr::mutate(  
    channel_management = case_when(
      ev1 == "Across top level management"|
        ev2 == "Across top level management"|
        ev3 == "Across top level management"|
        ev4 == "Across top level management"|
        ev5 == "Across top level management"|
        ev6 == "Across top level management"|
        ev7 == "Across top level management"|
        evu1 == "Across top level management"|
        evu2 == "Across top level management"|
        evu3 == "Across top level management"|
        evu4 == "Across top level management"|
        evu5 == "Across top level management"|
        evu6 == "Across top level management"|
        evu7 == "Across top level management"~ 1,   
    )) %>%
  dplyr::mutate(
    channel_management = as.numeric(channel_management)
  ) %>%
  dplyr::mutate(
    channel_management = case_when(
      is.na(channel_management) ~ 0,
      !is.na(channel_management) ~1)
  )


DMA1 <- DMA1 %>%
  dplyr::mutate(  
    channel_external = case_when(
      ev1 == "Outside of ING"|
        ev2 == "Outside of ING"|
        ev3 == "Outside of ING"|
        ev4 == "Outside of ING"|
        ev5 == "Outside of ING"|
        ev6 == "Outside of ING"|
        ev7 == "Outside of ING"|
        evu1 == "Outside of ING"|
        evu2 == "Outside of ING"|
        evu3 == "Outside of ING"|
        evu4 == "Outside of ING"|
        evu5 == "Outside of ING"|
        evu6 == "Outside of ING"|
        evu7 == "Outside of ING"~ 1,   
      )) %>%
  dplyr::mutate(
    channel_external = as.numeric(channel_external)
  ) %>%
  dplyr::mutate(
    channel_external = case_when(
      is.na(channel_external) ~ 0,
      !is.na(channel_external) ~1)
  )

DMA1 <- DMA1 %>%
  dplyr::mutate(
    channel_pointers = channel_team + channel_tribe + channel_across_tribes + channel_management + channel_external
  )