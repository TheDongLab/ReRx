##### use Pubchem URL to find generic name of all drugs

get_common_name <- function(drug_name) {
  url <- paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/name/", URLencode(drug_name), "/synonyms/XML")
  response <- GET(url)
  
  print(paste("Request URL:", url))
  print(paste("Status Code:", status_code(response)))
  
  if (status_code(response) == 200) {
    content_text <- content(response, as = "text", encoding = "UTF-8")
    print("API Response:")
    print(content_text)
    
    xml_data <- read_xml(content_text)
    
    print("Parsed XML:")
    print(xml_data)
    
    ns <- xml_ns(xml_data)
    print("Namespaces:")
    print(ns)
    
    synonyms_nodes <- xml_find_all(xml_data, ".//d1:Synonym", ns)
    print("Synonyms nodes with namespace:")
    print(synonyms_nodes)
    
    synonyms <- xml_text(synonyms_nodes)
    print("Extracted Synonyms:")
    print(synonyms)
    
    if (length(synonyms) > 0) {
      return(synonyms[1])
    } else {
      return(NA)
    }
  } else {
    return(NA)
  }
}

# An example of drug name extraction using get_common_name function (MGB Biobank)
# Med_all_with_Medication_extracted_unique & Med_all_with_Medication_extracted_unique could be found in Medication_MGB_Biobank.R
drug_names <- Med_all_with_Medication_extracted_unique$Medication_extracted_unique
common_names <- lapply(drug_names, get_common_name)
Med_all_with_Medication_extracted_unique$common_name <- unlist(common_names)