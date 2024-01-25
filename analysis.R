
# Design team
location <- DMA1 %>%
  dplyr::group_by(Year, country) %>%
  count(country) %>%
  dplyr::ungroup() %>%
  tidyr::spread(country, n) %>%
  replace(is.na(.), 0) %>%
  dplyr::mutate(total =  Australia + Belgium + Germany + Italy + Luxembourg + Netherlands + Philippines + Romania + Spain + Poland)

# Area 
Area <- DMA1 %>%
  dplyr::group_by(Year, country, `Which part of ING do you work for? `) %>%
  count(`Which part of ING do you work for? `) %>%
  dplyr::ungroup() %>%
  tidyr::spread(`Which part of ING do you work for? `, n) %>%
  replace(is.na(.), 0) %>%
  dplyr::filter(Year=="2023") %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(
    total = rowSums(across(where(is.numeric)))
  )


# Roles
roles_Interaction_UX_designer <- DMA1 %>%
  dplyr::group_by(Year, country, Interaction_UX_designer) %>%
  count(Interaction_UX_designer) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Interaction_UX_designer)) %>%
  dplyr::mutate(Interaction_UX_designer = n) %>%
  dplyr::select(-n)

roles_Service_Designer <- DMA1 %>%
  dplyr::group_by(Year, country, Service_designer) %>%
  count(Service_designer) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Service_designer)) %>%
  dplyr::mutate(Service_designer = n) %>%
  dplyr::select(-n)

roles_UX_researcher <- DMA1 %>%
  dplyr::group_by(Year, country, UX_researcher) %>%
  count(UX_researcher) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(UX_researcher)) %>%
  dplyr::mutate(UX_researcher = n) %>%
  dplyr::select(-n)

roles_UX_writer <- DMA1 %>%
  dplyr::group_by(Year, country, UX_writer) %>%
  count(UX_writer) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(UX_writer)) %>%
  dplyr::mutate(UX_writer = n) %>%
  dplyr::select(-n)

roles_UI_designer <- DMA1 %>%
  dplyr::group_by(Year, country, UI_designer) %>%
  count(UI_designer) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(UI_designer)) %>%
  dplyr::mutate(UI_designer = n) %>%
  dplyr::select(-n)

roles_Chapter_lead <- DMA1 %>%
  dplyr::group_by(Year, country, Chapter_lead) %>%
  count(Chapter_lead) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Chapter_lead)) %>%
  dplyr::mutate(Chapter_lead = n) %>%
  dplyr::select(-n)

roles_COE_lead <- DMA1 %>%
  dplyr::group_by(Year, country, COE_lead) %>%
  count(COE_lead) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(COE_lead)) %>%
  dplyr::mutate(COE_lead = n) %>%
  dplyr::select(-n)

roles <- dplyr::full_join(roles_Interaction_UX_designer, roles_Service_Designer)
roles <- dplyr::full_join(roles, roles_UX_researcher)
roles <- dplyr::full_join(roles, roles_UX_writer)
roles <- dplyr::full_join(roles, roles_UI_designer)
roles <- dplyr::full_join(roles, roles_Chapter_lead)
roles <- dplyr::full_join(roles, roles_COE_lead)


roles <- roles %>%
  replace(is.na(.), 0) %>%
  dplyr::mutate(
    total = rowSums(across(where(is.numeric)))
  )

# Feedback
review_feedback <- DMA1 %>%
  dplyr::group_by(Year, country, review_frequency) %>%
  count(review_frequency) %>%
  dplyr::ungroup() %>%
  tidyr::spread(review_frequency, n) %>%
  replace(is.na(.), 0) %>%
  dplyr::mutate(
    total = rowSums(across(where(is.numeric)))
  )

# involvement
involv <- DMA1 %>%
  dplyr::group_by(Year, country, design_involvement) %>%
  count(design_involvement) %>%
  dplyr::ungroup() %>%
  tidyr::spread(design_involvement, n) %>%
  replace(is.na(.), 0) %>%
  dplyr::mutate(
    total = rowSums(across(where(is.numeric)))
  )

# Desired workflow
flow <- DMA1 %>%
  dplyr::group_by(Year, country, desired_workflow) %>%
  count(desired_workflow) %>%
  dplyr::ungroup() %>%
  tidyr::spread(desired_workflow, n) %>%
  replace(is.na(.), 0) %>%
  dplyr::mutate(
    total = rowSums(across(where(is.numeric)))
  )

# training
training <- DMA1 %>%
  dplyr::group_by(Year, country, training) %>%
  count(training) %>%
  dplyr::ungroup() %>%
  tidyr::spread(training, n) %>%
  replace(is.na(.), 0) %>%
  dplyr::mutate(
    total = rowSums(across(where(is.numeric)))
  )

# Methodology

# research

tech_Accessibility_studies <- DMA1 %>%
  dplyr::group_by(Year, country, Accessibility_studies) %>%
  count(Accessibility_studies) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Accessibility_studies)) %>%
  dplyr::mutate(Accessibility_studies = n) %>%
  dplyr::select(-n)

Business_Product_Analytics_Review <- DMA1 %>%
  dplyr::group_by(Year, country, Business_Product_Analytics_Review) %>%
  count(Business_Product_Analytics_Review) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Business_Product_Analytics_Review)) %>%
  dplyr::mutate(Business_Product_Analytics_Review = n) %>%
  dplyr::select(-n)

Competitor_analysis <- DMA1 %>%
  dplyr::group_by(Year, country, Competitor_analysis) %>%
  count(Competitor_analysis) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Competitor_analysis)) %>%
  dplyr::mutate(Competitor_analysis = n) %>%
  dplyr::select(-n)

Concept_testing <- DMA1 %>%
  dplyr::group_by(Year, country, Concept_testing) %>%
  count(Concept_testing) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Concept_testing)) %>%
  dplyr::mutate(Concept_testing = n) %>%
  dplyr::select(-n)

Customer_journey_mapping <- DMA1 %>%
  dplyr::group_by(Year, country, Customer_journey_mapping) %>%
  count(Customer_journey_mapping) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Customer_journey_mapping)) %>%
  dplyr::mutate(Customer_journey_mapping = n) %>%
  dplyr::select(-n)

Field_research <- DMA1 %>%
  dplyr::group_by(Year, country, Field_research) %>%
  count(Field_research) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Field_research)) %>%
  dplyr::mutate(Field_research = n) %>%
  dplyr::select(-n)

Existing_research <- DMA1 %>%
  dplyr::group_by(Year, country, Existing_research) %>%
  count(Existing_research) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Existing_research)) %>%
  dplyr::mutate(Existing_research = n) %>%
  dplyr::select(-n)

Experimentation <- DMA1 %>%
  dplyr::group_by(Year, country, Experimentation) %>%
  count(Experimentation) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Experimentation)) %>%
  dplyr::mutate(Experimentation = n) %>%
  dplyr::select(-n)

Expert_reviews <- DMA1 %>%
  dplyr::group_by(Year, country, Expert_reviews) %>%
  count(Expert_reviews) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Expert_reviews)) %>%
  dplyr::mutate(Expert_reviews = n) %>%
  dplyr::select(-n)

Focus_groups <- DMA1 %>%
  dplyr::group_by(Year, country, Focus_groups) %>%
  count(Focus_groups) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Focus_groups)) %>%
  dplyr::mutate(Focus_groups = n) %>%
  dplyr::select(-n)

Interviews <- DMA1 %>%
  dplyr::group_by(Year, country, Interviews) %>%
  count(Interviews) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Interviews)) %>%
  dplyr::mutate(Interviews = n) %>%
  dplyr::select(-n)

Information_Architecture <- DMA1 %>%
  dplyr::group_by(Year, country, Information_Architecture) %>%
  count(Information_Architecture) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Information_Architecture)) %>%
  dplyr::mutate(Information_Architecture = n) %>%
  dplyr::select(-n)

Persona <- DMA1 %>%
  dplyr::group_by(Year, country, Persona) %>%
  count(Persona) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Persona)) %>%
  dplyr::mutate(Persona = n) %>%
  dplyr::select(-n)

Quali_Usability_testing <- DMA1 %>%
  dplyr::group_by(Year, country, Quali_Usability_testing) %>%
  count(Quali_Usability_testing) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Quali_Usability_testing)) %>%
  dplyr::mutate(Quali_Usability_testing = n) %>%
  dplyr::select(-n)

Quanti_Attitudinal_test <- DMA1 %>%
  dplyr::group_by(Year, country, Quanti_Attitudinal_test) %>%
  count(Quanti_Attitudinal_test) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Quanti_Attitudinal_test)) %>%
  dplyr::mutate(Quanti_Attitudinal_test = n) %>%
  dplyr::select(-n)

Quanti_Usability_testing <- DMA1 %>%
  dplyr::group_by(Year, country, Quanti_Usability_testing) %>%
  count(Quanti_Usability_testing) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Quanti_Usability_testing)) %>%
  dplyr::mutate(Quanti_Usability_testing = n) %>%
  dplyr::select(-n)

Stakeholder_interviews <- DMA1 %>%
  dplyr::group_by(Year, country, Stakeholder_interviews) %>%
  count(Stakeholder_interviews) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Stakeholder_interviews)) %>%
  dplyr::mutate(Stakeholder_interviews = n) %>%
  dplyr::select(-n)

Sentiment_analysis <- DMA1 %>%
  dplyr::group_by(Year, country, Sentiment_analysis) %>%
  count(Sentiment_analysis) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Sentiment_analysis)) %>%
  dplyr::mutate(Sentiment_analysis = n) %>%
  dplyr::select(-n)

Service_Blueprinting <- DMA1 %>%
  dplyr::group_by(Year, country, Service_Blueprinting) %>%
  count(Service_Blueprinting) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Service_Blueprinting)) %>%
  dplyr::mutate(Service_Blueprinting = n) %>%
  dplyr::select(-n)


Survey <- DMA1 %>%
  dplyr::group_by(Year, country, Survey) %>%
  count(Survey) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Survey)) %>%
  dplyr::mutate(Survey = n) %>%
  dplyr::select(-n)

research_techniques <- dplyr::full_join(tech_Accessibility_studies, Business_Product_Analytics_Review)
research_techniques <- dplyr::full_join(research_techniques, Competitor_analysis)
research_techniques <- dplyr::full_join(research_techniques, Concept_testing)
research_techniques <- dplyr::full_join(research_techniques, Field_research)
research_techniques <- dplyr::full_join(research_techniques, Existing_research)
research_techniques <- dplyr::full_join(research_techniques, Experimentation)
research_techniques <- dplyr::full_join(research_techniques, Expert_reviews)
research_techniques <- dplyr::full_join(research_techniques, Focus_groups)
research_techniques <- dplyr::full_join(research_techniques, Interviews)
research_techniques <- dplyr::full_join(research_techniques, Information_Architecture)
research_techniques <- dplyr::full_join(research_techniques, Persona)
research_techniques <- dplyr::full_join(research_techniques, Quali_Usability_testing)
research_techniques <- dplyr::full_join(research_techniques, Quanti_Attitudinal_test)
research_techniques <- dplyr::full_join(research_techniques, Quanti_Usability_testing)
research_techniques <- dplyr::full_join(research_techniques, Stakeholder_interviews)
research_techniques <- dplyr::full_join(research_techniques, Sentiment_analysis)
research_techniques <- dplyr::full_join(research_techniques, Service_Blueprinting)
research_techniques <- dplyr::full_join(research_techniques, Survey)


research_techniques <- research_techniques %>%
  replace(is.na(.), 0) %>%
  dplyr::mutate(
    total = rowSums(across(where(is.numeric)))
  ) %>%
  t()


# design techniques
Design_review <- DMA1 %>%
  dplyr::group_by(Year, country, Design_review) %>%
  count(Design_review) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Design_review)) %>%
  dplyr::mutate(Design_review = n) %>%
  dplyr::select(-n)

Ideation_workshops <- DMA1 %>%
  dplyr::group_by(Year, country, Ideation_workshops) %>%
  count(Ideation_workshops) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Ideation_workshops)) %>%
  dplyr::mutate(Ideation_workshops = n) %>%
  dplyr::select(-n)

Content_writing <- DMA1 %>%
  dplyr::group_by(Year, country, Content_writing) %>%
  count(Content_writing) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Content_writing)) %>%
  dplyr::mutate(Content_writing = n) %>%
  dplyr::select(-n)

Motion_design <- DMA1 %>%
  dplyr::group_by(Year, country, Motion_design) %>%
  count(Motion_design) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Motion_design)) %>%
  dplyr::mutate(Motion_design = n) %>%
  dplyr::select(-n)

Illustration_design <- DMA1 %>%
  dplyr::group_by(Year, country, Illustration_design) %>%
  count(Illustration_design) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Illustration_design)) %>%
  dplyr::mutate(Illustration_design = n) %>%
  dplyr::select(-n)

Accessibility_review <- DMA1 %>%
  dplyr::group_by(Year, country, Accessibility_review) %>%
  count(Accessibility_review) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Accessibility_review)) %>%
  dplyr::mutate(Accessibility_review = n) %>%
  dplyr::select(-n)

User_flow_diagramming <- DMA1 %>%
  dplyr::group_by(Year, country, User_flow_diagramming) %>%
  count(User_flow_diagramming) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(User_flow_diagramming)) %>%
  dplyr::mutate(User_flow_diagramming = n) %>%
  dplyr::select(-n)

Site_mapping <- DMA1 %>%
  dplyr::group_by(Year, country, Site_mapping) %>%
  count(Site_mapping) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Site_mapping)) %>%
  dplyr::mutate(Site_mapping = n) %>%
  dplyr::select(-n)

Wireframing <- DMA1 %>%
  dplyr::group_by(Year, country, Wireframing) %>%
  count(Wireframing) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Wireframing)) %>%
  dplyr::mutate(Wireframing = n) %>%
  dplyr::select(-n)

prototyping <- DMA1 %>%
  dplyr::group_by(Year, country, prototyping) %>%
  count(prototyping) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(prototyping)) %>%
  dplyr::mutate(prototyping = n) %>%
  dplyr::select(-n)

High_prototyping <- DMA1 %>%
  dplyr::group_by(Year, country, High_prototyping) %>%
  count(High_prototyping) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(High_prototyping)) %>%
  dplyr::mutate(High_prototyping = n) %>%
  dplyr::select(-n)

Code_level_prototyping <- DMA1 %>%
  dplyr::group_by(Year, country, Code_level_prototyping) %>%
  count(Code_level_prototyping) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Code_level_prototyping)) %>%
  dplyr::mutate(Code_level_prototyping = n) %>%
  dplyr::select(-n)

Service_simulations <- DMA1 %>%
  dplyr::group_by(Year, country, Service_simulations) %>%
  count(Service_simulations) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Service_simulations)) %>%
  dplyr::mutate(Service_simulations = n) %>%
  dplyr::select(-n)

Feasibility_review <- DMA1 %>%
  dplyr::group_by(Year, country, Feasibility_review) %>%
  count(Feasibility_review) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Feasibility_review)) %>%
  dplyr::mutate(Feasibility_review = n) %>%
  dplyr::select(-n)

Customer_creation <- DMA1 %>%
  dplyr::group_by(Year, country, Customer_creation) %>%
  count(Customer_creation) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Customer_creation)) %>%
  dplyr::mutate(Customer_creation = n) %>%
  dplyr::select(-n)

Design_sprint <- DMA1 %>%
  dplyr::group_by(Year, country, Design_sprint) %>%
  count(Design_sprint) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Design_sprint)) %>%
  dplyr::mutate(Design_sprint = n) %>%
  dplyr::select(-n)

design_techniques <- dplyr::full_join(Design_review, Ideation_workshops)
design_techniques <- dplyr::full_join(design_techniques, Content_writing)
design_techniques <- dplyr::full_join(design_techniques, Motion_design)
design_techniques <- dplyr::full_join(design_techniques, Illustration_design)
design_techniques <- dplyr::full_join(design_techniques, Accessibility_review)
design_techniques <- dplyr::full_join(design_techniques, User_flow_diagramming)
design_techniques <- dplyr::full_join(design_techniques, Site_mapping)
design_techniques <- dplyr::full_join(design_techniques, Wireframing)
design_techniques <- dplyr::full_join(design_techniques, prototyping)
design_techniques <- dplyr::full_join(design_techniques, High_prototyping)
design_techniques <- dplyr::full_join(design_techniques, Code_level_prototyping)
design_techniques <- dplyr::full_join(design_techniques, Service_simulations)
design_techniques <- dplyr::full_join(design_techniques, Feasibility_review)
design_techniques <- dplyr::full_join(design_techniques, Customer_creation)
design_techniques <- dplyr::full_join(design_techniques, Design_sprint)


design_techniques <- design_techniques %>%
  replace(is.na(.), 0) %>%
  dplyr::mutate(
    total = rowSums(across(where(is.numeric)))
  ) %>%
  t()

#metrics
Metrics_usage <- DMA1 %>%
  dplyr::group_by(Year, country, Metrics_usage) %>%
  count(Metrics_usage) %>%
  dplyr::ungroup() %>%
  tidyr::spread(Metrics_usage, n) %>%
  replace(is.na(.), 0) %>%
  dplyr::mutate(
    total = rowSums(across(where(is.numeric)))
  )

#processes
processes <- DMA1 %>%
  dplyr::group_by(Year, country, processes_consistency) %>%
  count(processes_consistency) %>%
  dplyr::ungroup() %>%
  tidyr::spread(processes_consistency, n) %>%
  replace(is.na(.), 0) %>%
  dplyr::mutate(
    total = rowSums(across(where(is.numeric)))
  )


## Stakeholder collaboration
stake_accessability <- DMA1 %>%
  dplyr::group_by(Year, country, stakeholders_Accessibility_team) %>%
  count(stakeholders_Accessibility_team) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(stakeholders_Accessibility_team)) %>%
  dplyr::mutate(stakeholders_Accessibility_team = n) %>%
  dplyr::select(-n)

stakeholders_marketing <- DMA1 %>%
  dplyr::group_by(Year, country, stakeholders_marketing) %>%
  count(stakeholders_marketing) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(stakeholders_marketing)) %>%
  dplyr::mutate(stakeholders_marketing = n) %>%
  dplyr::select(-n)

stakeholders_coaches <- DMA1 %>%
  dplyr::group_by(Year, country, stakeholders_coaches) %>%
  count(stakeholders_coaches) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(stakeholders_coaches)) %>%
  dplyr::mutate(stakeholders_coaches = n) %>%
  dplyr::select(-n)

stakeholders_CX <- DMA1 %>%
  dplyr::group_by(Year, country, stakeholders_CX) %>%
  count(stakeholders_CX) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(stakeholders_CX)) %>%
  dplyr::mutate(stakeholders_CX = n) %>%
  dplyr::select(-n)

stakeholders_CJE <- DMA1 %>%
  dplyr::group_by(Year, country, stakeholders_CJE) %>%
  count(stakeholders_CJE) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(stakeholders_CJE)) %>%
  dplyr::mutate(stakeholders_CJE = n) %>%
  dplyr::select(-n)

stakeholders_Contact_center <- DMA1 %>%
  dplyr::group_by(Year, country, stakeholders_Contact_center) %>%
  count(stakeholders_Contact_center) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(stakeholders_Contact_center)) %>%
  dplyr::mutate(stakeholders_Contact_center = n) %>%
  dplyr::select(-n)

stakeholders_legal <- DMA1 %>%
  dplyr::group_by(Year, country, stakeholders_legal) %>%
  count(stakeholders_legal) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(stakeholders_legal)) %>%
  dplyr::mutate(stakeholders_legal = n) %>%
  dplyr::select(-n)

stakeholders_PO <- DMA1 %>%
  dplyr::group_by(Year, country, stakeholders_PO) %>%
  count(stakeholders_PO) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(stakeholders_PO)) %>%
  dplyr::mutate(stakeholders_PO = n) %>%
  dplyr::select(-n)

stakeholders_risk <- DMA1 %>%
  dplyr::group_by(Year, country, stakeholders_risk) %>%
  count(stakeholders_risk) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(stakeholders_risk)) %>%
  dplyr::mutate(stakeholders_risk = n) %>%
  dplyr::select(-n)

stakeholders_Senior_management <- DMA1 %>%
  dplyr::group_by(Year, country, stakeholders_Senior_management) %>%
  count(stakeholders_Senior_management) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(stakeholders_Senior_management)) %>%
  dplyr::mutate(stakeholders_Senior_management = n) %>%
  dplyr::select(-n)

stakeholders_IT <- DMA1 %>%
  dplyr::group_by(Year, country, stakeholders_IT) %>%
  count(stakeholders_IT) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(stakeholders_IT)) %>%
  dplyr::mutate(stakeholders_IT = n) %>%
  dplyr::select(-n)

stakeholders_Third_parties <- DMA1 %>%
  dplyr::group_by(Year, country, stakeholders_Third_parties) %>%
  count(stakeholders_Third_parties) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(stakeholders_Third_parties)) %>%
  dplyr::mutate(stakeholders_Third_parties = n) %>%
  dplyr::select(-n)

stakeholders_Development <- DMA1 %>%
  dplyr::group_by(Year, country, stakeholders_Development) %>%
  count(stakeholders_Development) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(stakeholders_Development)) %>%
  dplyr::mutate(stakeholders_Development = n) %>%
  dplyr::select(-n)

stakeholderss <- dplyr::full_join(stake_accessability, stakeholders_marketing)
stakeholderss <- dplyr::full_join(stakeholderss, stakeholders_CX)
stakeholderss <- dplyr::full_join(stakeholderss, stakeholders_CJE)
stakeholderss <- dplyr::full_join(stakeholderss, stakeholders_coaches)
stakeholderss <- dplyr::full_join(stakeholderss, stakeholders_Contact_center)
stakeholderss <- dplyr::full_join(stakeholderss, stakeholders_legal)
stakeholderss <- dplyr::full_join(stakeholderss, stakeholders_PO)
stakeholderss <- dplyr::full_join(stakeholderss, stakeholders_risk)
stakeholderss <- dplyr::full_join(stakeholderss, stakeholders_Senior_management)
stakeholderss <- dplyr::full_join(stakeholderss, stakeholders_IT)
stakeholderss <- dplyr::full_join(stakeholderss, stakeholders_Third_parties)
stakeholderss <- dplyr::full_join(stakeholderss, stakeholders_Development)

stakeholderss <- stakeholderss %>%
  replace(is.na(.), 0) %>%
  dplyr::mutate(
    total = rowSums(across(where(is.numeric)))
  )

## non design colleagues
colla_non_design <- DMA1 %>%
  dplyr::group_by(Year, country, collaboration) %>%
  count(collaboration) %>%
  dplyr::ungroup() %>%
  tidyr::spread(collaboration, n) %>%
  replace(is.na(.), 0) %>%
  dplyr::mutate(
    total = rowSums(across(where(is.numeric)))
  )



## activities
activities_squad <- DMA1 %>%
  dplyr::group_by(Year, country, activities_squad) %>%
  count(activities_squad) %>%
  dplyr::ungroup() %>%
  dplyr::filter(activities_squad!=0) %>%
  dplyr::mutate(activities_squad = n) %>%
  dplyr::select(-n)

activities_tribe <- DMA1 %>%
  dplyr::group_by(Year, country, activities_tribe) %>%
  count(activities_tribe) %>%
  dplyr::ungroup() %>%
  dplyr::filter(activities_tribe!=0) %>%
  dplyr::mutate(activities_tribe = n) %>%
  dplyr::select(-n)

activities_Q <- DMA1 %>%
  dplyr::group_by(Year, country, activities_Q) %>%
  count(activities_Q) %>%
  dplyr::ungroup() %>%
  dplyr::filter(activities_Q!=0) %>%
  dplyr::mutate(activities_Q = n) %>%
  dplyr::select(-n)

activities_non <- DMA1 %>%
  dplyr::group_by(Year, country, activities_non) %>%
  count(activities_non) %>%
  dplyr::ungroup() %>%
  dplyr::filter(activities_non!=0) %>%
  dplyr::mutate(activities_non = n) %>%
  dplyr::select(-n)

activities_not_sure <- DMA1 %>%
  dplyr::group_by(Year, country, activities_not_sure) %>%
  count(activities_not_sure) %>%
  dplyr::ungroup() %>%
  dplyr::filter(activities_not_sure!=0) %>%
  dplyr::mutate(activities_not_sure = n) %>%
  dplyr::select(-n)

activitiess <- dplyr::full_join(activities_squad, activities_tribe)
activitiess <- dplyr::full_join(activitiess, activities_Q)
activitiess <- dplyr::full_join(activitiess, activities_non)
activitiess <- dplyr::full_join(activitiess, activities_not_sure)

activitiess <- activitiess %>%
  replace(is.na(.), 0) %>%
  dplyr::mutate(
    total = rowSums(across(where(is.numeric)))
  )

# improvement
improvement <- DMA1 %>%
  dplyr::group_by(Year, country, improvement) %>%
  count(improvement) %>%
  dplyr::ungroup() %>%
  tidyr::spread(improvement, n) %>%
  replace(is.na(.), 0) %>%
  dplyr::mutate(
    total = rowSums(across(where(is.numeric)))
  )


customer_focus <- DMA1 %>%
  dplyr::group_by(Year, country, customer_focus) %>%
  count(customer_focus) %>%
  dplyr::ungroup() %>%
  tidyr::spread(customer_focus, n)  %>%
  replace(is.na(.), 0) %>%
  dplyr::mutate(
    total = rowSums(across(where(is.numeric)))
  )

user_research_insightss <- DMA1 %>%
  dplyr::group_by(Year, country, user_research_insightss) %>%
  count(user_research_insightss) %>%
  dplyr::ungroup() %>%
  tidyr::spread(user_research_insightss, n)  %>%
  replace(is.na(.), 0) %>%
  dplyr::mutate(
    total = rowSums(across(where(is.numeric)))
  )

financial_resources <- DMA1 %>%
  dplyr::group_by(Year, country, financial_resources) %>%
  count(financial_resources) %>%
  dplyr::ungroup() %>%
  tidyr::spread(financial_resources, n)  %>%
  replace(is.na(.), 0) %>%
  dplyr::mutate(
    total = rowSums(across(where(is.numeric)))
  )

leadership_support <- DMA1 %>%
  dplyr::group_by(Year, country, leadership_support) %>%
  count(leadership_support) %>%
  dplyr::ungroup() %>%
  tidyr::spread(leadership_support, n)  %>%
  replace(is.na(.), 0) %>%
  dplyr::mutate(
    total = rowSums(across(where(is.numeric)))
  )

it_business_alignment <- DMA1 %>%
  dplyr::group_by(Year, country, it_business_alignment) %>%
  count(it_business_alignment) %>%
  dplyr::ungroup() %>%
  tidyr::spread(it_business_alignment, n)  %>%
  replace(is.na(.), 0) %>%
  dplyr::mutate(
    total = rowSums(across(where(is.numeric)))
  )

innovation_opportunity <- DMA1 %>%
  dplyr::group_by(Year, country, innovation_opportunity) %>%
  count(innovation_opportunity) %>%
  dplyr::ungroup() %>%
  tidyr::spread(innovation_opportunity, n)  %>%
  replace(is.na(.), 0) %>%
  dplyr::mutate(
    total = rowSums(across(where(is.numeric)))
  )

# channel
channel_across_tribes <- DMA1 %>%
  dplyr::group_by(Year, country, channel_across_tribes) %>%
  count(channel_across_tribes) %>%
  dplyr::ungroup() %>%
  dplyr::filter(channel_across_tribes!=0) %>%
  dplyr::mutate(channel_across_tribes = n) %>%
  dplyr::select(-n)

channel_not_sure <- DMA1 %>%
  dplyr::group_by(Year, country, channel_not_sure) %>%
  count(channel_not_sure) %>%
  dplyr::ungroup() %>%
  dplyr::filter(channel_not_sure!=0) %>%
  dplyr::mutate(channel_not_sure = n) %>%
  dplyr::select(-n)

channel_dont <- DMA1 %>%
  dplyr::group_by(Year, country, channel_dont) %>%
  count(channel_dont) %>%
  dplyr::ungroup() %>%
  dplyr::filter(channel_dont!=0) %>%
  dplyr::mutate(channel_dont = n) %>%
  dplyr::select(-n)

channel_team <- DMA1 %>%
  dplyr::group_by(Year, country, channel_team) %>%
  count(channel_team) %>%
  dplyr::ungroup() %>%
  dplyr::filter(channel_team!=0) %>%
  dplyr::mutate(channel_team = n) %>%
  dplyr::select(-n)

channel_external <- DMA1 %>%
  dplyr::group_by(Year, country, channel_external) %>%
  count(channel_external) %>%
  dplyr::ungroup() %>%
  dplyr::filter(channel_external!=0) %>%
  dplyr::mutate(channel_external = n) %>%
  dplyr::select(-n)

channel_tribe <- DMA1 %>%
  dplyr::group_by(Year, country, channel_tribe) %>%
  count(channel_tribe) %>%
  dplyr::ungroup() %>%
  dplyr::filter(channel_tribe!=0) %>%
  dplyr::mutate(channel_tribe = n) %>%
  dplyr::select(-n)

channel_management <- DMA1 %>%
  dplyr::group_by(Year, country, channel_management) %>%
  count(channel_management) %>%
  dplyr::ungroup() %>%
  dplyr::filter(channel_management!=0) %>%
  dplyr::mutate(channel_management = n) %>%
  dplyr::select(-n)

channels <- dplyr::full_join(channel_management, channel_tribe)
channels <- dplyr::full_join(channels, channel_external)
channels <- dplyr::full_join(channels, channel_team)
channels <- dplyr::full_join(channels, channel_dont)
channels <- dplyr::full_join(channels, channel_across_tribes)
channels <- dplyr::full_join(channels, channel_not_sure)

channels <- channels %>%
  replace(is.na(.), 0) %>%
  dplyr::mutate(
    total = rowSums(across(where(is.numeric)))
  )


### totale score

total <- DMA1 %>%
  dplyr::mutate(
    total_team = (review_frequency + desired_workflow + training + design_involvement) * (5/18)
  ) %>%
  dplyr::mutate(
    total_methodology= (UX_applied + dt_applied_points + Metrics_usage) * (5/43)
  ) %>%
  dplyr::mutate(
    total_governance= (processes_consistency + nd_stakeholders_points + collaboration + activities_pointers + improvement) * (5/29)
  ) %>%
  dplyr::mutate(
    total_strategy= 
      (user_research_insightss + customer_focus + financial_resources) * (5/13)
  ) %>%
  dplyr::mutate(
    total_culture= 
      (leadership_support + innovation_opportunity + it_business_alignment + channel_pointers) * (5/21)
  )

total1 <- total %>%
  dplyr::group_by(Year, country) %>%
  dplyr::summarise(
    total_team = mean(total_team),
    total_methodology = mean(total_methodology),
    total_governance = mean(total_governance),
    total_strategy = mean(total_strategy),
    total_culture = mean(total_culture),
  )

total1 <- total %>%
  dplyr::group_by(Year, country) %>%
  dplyr::summarise(
    total_team = round(mean(total_team), digits = 2),
    total_methodology = round(mean(total_methodology), digits = 2),
    total_governance = round(mean(total_governance), digits = 2),
    total_strategy = round(mean(total_strategy), digits = 2),
    total_culture = round(mean(total_culture), digits = 2),
  )

