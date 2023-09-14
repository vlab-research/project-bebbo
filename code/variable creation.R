#############################################################
# Variable creation
#############################################################

key <- read_csv("../data/raw/Bebbo Evaluation Survey - Serbia - English Analysis notes - Baseline.csv")

key[which(key$variable=='past_24h_play'),'Correct']<-'Yes'

# key <- parse_multi_choice(key)

all_vars <- key %>%
  filter(!is.na(Domain)) %>%
  pull(variable)

#create a map of correct responses for two variables
binary_confs <- list(
  bin_conf("download_confirm_treatment", "Yes"),
  bin_conf("using_bebbo", "Yes")
)

#create a map of correct responses for each construct variable
likert_cols<-key%>%
  select(c('variable','Correct'))%>%
  rowwise()%>%
  mutate(check=length(parse_mult_choice(Correct)))%>%
  ungroup()%>%
  filter(check>1)%>%
  pull(variable)

binarized_vars <- key %>%
  filter(!variable %in% likert_cols) %>%
  filter(!is.na(construct_variable)) %>%
  rowwise() %>%
  mutate(foo = list(bin_conf(variable, Correct))) %>%
  pull(foo)

likert_confs <- key %>%
  filter(variable %in% likert_cols) %>%
  filter(!is.na(construct_variable)) %>%
  rowwise() %>%
  mutate(foo = list(likert_conf(variable, Correct, answers))) %>%
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
