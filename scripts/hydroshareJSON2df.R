
# Load packages
library("tidyverse")
library("httr")
library("jsonlite")


# Try accessing the API directly with R
# Helpful: https://www.programmableweb.com/news/how-to-access-any-restful-api-using-r-language/how-to/2017/07/21
# username and password
username = "dustinkincaid"
password = "mow9gug-WED"

# Provide pieces of the url needed to access the API
# Here is the base url
base <- "https://www.hydroshare.org/"

# Here is the information for the resource search (see https://www.hydroshare.org/hsapi/?format=openapi for this information)
res_search <- "/hsapi/resource/"
# Here are the search terms I would like to use in the subject field
# sub1 = "runoff"
# sub2 = "dishcarge"
# sub3 = "streamflow"

# Make the API call to /hsapi/resource/
  # Initialize the variables/paste together the necessary url
  # Ultimate url: "https://www.hydroshare.org/hsapi/resource/?subject=runoff%2C%20streamflow%2C%20discharge"
  # call_res_search <- paste(base, res_search, "?", "subject", "=", sub1, "%2C%20", sub2, "%2C%20", sub3, sep = "")
  
  # The above didn't work, so I pulled the url from the subject search I did directly on the Hydroshare API 
  call_res_search <- "https://www.hydroshare.org/hsapi/resource/?subject=runoff%2C%20streamflow%2C%20discharge&edit_permission=false&published=false&include_obsolete=false"
  # Now execute the call using the GET function of httr
  get_res <- GET(call_res_search, authenticate(username, password, type = "basic"))
  
# Tidy up the response from the API
  # Deserialize the response to get the raw data
  get_res_text <- content(get_res, "text")
  # Convert the raw data from the API to JSON format
  get_res_json <- fromJSON(get_res_text, simplifyDataFrame = FALSE)

# Parse the JSON using jsonlite
# Help from here: https://tidyr.tidyverse.org/articles/rectangle.html
  # Convert the results element (4th element) of the nested list to a tibble
  resources <- tibble(dataset = get_res_json[[4]])
  
  # View the components of each dataset
  names(resources$dataset[[1]])

  # Here we can takes every component and make a new column
  # test <- resources %>% unnest_wider(dataset)

  # Or we can use hoist() to pull out only the components we want
  resources <- resources %>% hoist(dataset,
                              type = "resource_type",
                              title = "resource_title",
                              id = "resource_id",
                              url = "resource_url",
                              url_meta = "science_metadata_url",
                              abstract = "abstract") %>% 
    # Select only the columns you want to keep to drop all other nested lists/columns remaining
    select(type, title, id, url, url_meta, abstract)



# Now try getting the metadata from and API call to /hsapi/resource/{id}/scimeta/elements/
 # Here is an example with a SINGLE example id (ultimately want to loop through all the ids in resources df above)
  md_search <- "/hsapi/resource/5ef99335b3ac4066b75c7f205dee40e9/scimeta/elements/"

  # Initialize the variables/paste together the necessary url
  # Ultimate url: "https://www.hydroshare.org/hsapi/resource/{id}/scimeta/elements/"
  call_md_search <- paste(base, md_search, sep = "")
  # Execute the call
  get_md <- GET(call_md_search, authenticate(username, password, type = "basic"))
  
# Question 1: How do I extract the 'title' and 'subjects' from this single dataset such that I get a tibble or dataframe like:
# A)
#   title   subject
#   title1  discharge
#   title1  USGS
  
# OR
  
# B)
#   title   subject1  subject2  subjectX
#   title1  discharge USGS      NA
  
# Keep in mind each dataset will have a different number of subject values
  
# Question 2: How do I write a loop to get all of the metadata from all of the 'ids' in the 'resources' df above and extract
# the title and subjects from each one of them and put them in a nice tibble or dataframe?

# Get all subjects from the metadata for each one of the resources in 'resources'
# Loop to get the subjects from the API (from Pat Clemins)
getSubjectsDF <- function(resourceID) {
  md_search <- paste("/hsapi/resource/",resourceID,"/scimeta/elements/", sep="")
  print(md_search)
  
  # Initialize the variables/paste together the necessary url
  # Ultimate url: "https://www.hydroshare.org/hsapi/resource/{id}/scimeta/elements/"
  call_md_search <- paste(base, md_search, sep = "")
  # Execute the call
  get_md <- GET(call_md_search, authenticate(username, password, type = "basic"))
  get_md_JSON <- fromJSON(content(get_md, "text"), simplifyDataFrame = FALSE)
  df = data.frame(title=get_md_JSON$title, subject=get_md_JSON$subjects, stringsAsFactors = FALSE)
}

# Use lapply to loop through the results and call getSubjectsDF() on each result
list_of_DFs <- lapply(1:length(get_res_json$results), function(i) {getSubjectsDF(get_res_json$results[[i]]$resource_id)})

# Use Reduce and merge to take that list of dataframes and smash them into one
oneDF <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, all=TRUE), list_of_DFs)

# Filter the resources for those that include discharge, runoff, or streamflow
test <- oneDF %>% 
  filter_at(vars(contains("subject")), any_vars(str_detect(tolower(.), pattern = "discharge|streamflow|runoff")))
  filter_at(vars(contains("subject")), any_vars(str_detect(tolower(.), pattern != "specific |streamflow|runoff")))
  # filter_all(any_vars(str_detect(tolower(.), pattern = "discharge")))

subjects <- oneDF %>% 
  pivot_longer(cols = -title, names_to = "subjectNo", values_to = "subject") %>% 
  filter(!is.na(subject)) %>% 
  mutate(subject = str_to_lower(subject)) %>% 
  group_by(title, subject) %>%
  distinct() %>% 
  ungroup() %>% 
  mutate(subject = replace(subject, subject %in% c("streamflow / discharge", "streamflow"), "discharge"),
         subject = replace(subject, subject %in% c("temperature", "stream water temperatures"), "stream temperature")) %>% 
  group_by(subject) %>% 
  tally() %>% 
  arrange(desc(n))


