#############################################################
# Variable creation
#############################################################

key <- read_csv("data/raw/Bebbo Evaluation Survey - Serbia - English Analysis notes - Baseline.csv")

key[which(key$variable=='past_24h_play'),'Correct']<-'Yes'

key <- parse_multi_choice(key)

all_vars <- key %>%
  filter(!is.na(Domain)) %>%
  pull(variable)

#create a map of correct responses for two variables
binary_confs <- list(
  bin_conf("download_confirm_treatment", "Yes"),
  bin_conf("using_bebbo", "Yes")
)

#create a map of correct responses for each construct variable
binarized_vars <- key %>%
  filter(!is.na(construct_variable)) %>%
  rowwise() %>%
  mutate(foo = list(bin_conf(variable, c(Correct,Correct2,Correct3)))) %>% #if response contains values from any of the target columns
  pull(foo)

#final map
binary_confs <- c(binary_confs, binarized_vars)

#constructs
s <- key %>%
  filter(!is.na(construct_variable)) %>%
  pull(construct_variable) %>%
  unique()

#grab all variables associated with the constructs in s
ss <- sapply(s, function(x) {
  key %>%
    filter(construct_variable == x) %>%
    pull(variable)})


#ID columns

survey_cols<-c("surveyid",
               "creative",
               "survey_name",
               "form_language",
               "form_lang",
               "form_wave",
               "form_test",
               "shortcode",
               "userid",
               "version",
               "seed",
               "survey_start_time",
               "survey_end_time",
               "survey_created",
               "survey_duration",
               "answer_time_75",
               "answer_time_90",
               "answer_time_median",
               "answer_time_min")

question_cols<-c("alphabet",
                 "articles",
                 "breastfed",
                 "was_breastfed",
                 "concern_12mo",
                 "concern_3yo",
                 "concern_5yo",
                 "confidence_deal_emotions",
                 "confidence_respond_misbehave",
                 "count_to_ten",
                 "cry_separated",
                 "decrease_stress",
                 "express_feelings",
                 "games",
                 "healthy_food_groups",
                 "improve_family",
                 "joke_with_child",
                 "know_cog_dev",
                 "know_lang_dev",
                 "know_name_age",
                 "know_phys_dev",
                 "know_social_emotional_dev",
                 "know_when_vaccine",
                 "know_which_vaccine",
                 "laugh_together",
                 "lose_patience_punish",
                 "make_fun_of",
                 "meaning_of_no",
                 "movement",
                 "multiplication",
                 "name_colors",
                 "other_features",
                 "parenting_stress_1",
                 "parenting_stress_2",
                 "past_24h_draw",
                 "past_24h_outside",
                 "past_24h_play",
                 "past_24h_read",
                 "past_24h_sing",
                 "past_24h_stories",
                 "practices_24",
                 "physical_punishment",
                 "play_on_floor",
                 "relationship_to_child",
                 "say_name_age",
                 "personal_needs",
                 "scribble",
                 "simple_songs",
                 "sit_support",
                 "smile_around_child",
                 "snap_at_child",
                 "sort_pair",
                 "tell_story",
                 "threaten",
                 "track_checkups",
                 "track_dev",
                 "track_growth",
                 "track_vax")

parent_cols<-c("parent_age",
               "parent_gender",
               "education",
               "language",
               "location",
               "number_children",
               "using_bebbo",
               "child_0-6",
               "child_age",
               "child_name",
               "children_registered",
               "how_often_use",
               "how_long_use",
               "gender")


other_cols<- c("consent",
               "control_bebbo_knowledge",
               "control_bebbo_usage",
               "default_tys",
               "download_confirm_control",
               "download_confirm_treatment",
               "followup_permission",
               "hello_again",
               "intro_1",
               "intro_1b_control",
               "intro_1b_treatment",
               "intro_3",
               "payment_processing",
               "thankyou_1",
               "thankyou_early",
               "thankyou_optout",
               "thankyou_you_qualify",
               "seed_2",
               "treatment",
               "endline",
               "health_knw",
               "dev_knw_recog",
               "caregiver_well_being",
               "dev_knw_concern_0_2")
