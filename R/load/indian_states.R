
# Indian states by zone
Indian_states <- list(IAn = c("Chandigarh", "Delhi", "New Delhi", 
                              "Haryana", "Himachal Pradesh",
                              "Jammu and Kashmir", "Jammu", "Punjab", "Rajasthan",
                              "Uttarakhand", "Uttaranchal"),
                      
                      IAc = c("Chhattisgarh", "Madhya Pradesh", "Uttar Pradesh"),
                      
                      IAe = c("Bihar", "Jharkhand", "Odisha", "Orissa", "West Bengal"),
                      
                      IAne = c("Arunachal Pradesh", "ArunachalPradesh", 
                               "Assam", "Manipur", "Meghalaya",
                               "Mizoram", "Nagaland", "Sikkim", "Tripura"),
                      
                      IAw = c("Dadra and Nagar Haveli", "Daman and Diu",
                              "Goa", "Gujarat", "Maharashtra"),
                      
                      IAs = c("Andaman and Nicobar Islands", "Andhra Pradesh",
                              "Karnataka", "Kerala", "Lakshadweep", "Puducherry", 
                              "Tamil Nadu", "Telangana"))

Indian_states <- unlist(Indian_states, use.names = T)

Indian_states <- data.table(v024=tolower(Indian_states), 
                            region=gsub("[[:digit:]]*$", "", names(Indian_states) ) )
