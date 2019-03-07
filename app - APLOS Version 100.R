######################################################################################################################################
#                                                                                                                                    #
# Usage:                                                                                                                             #
#     Download API key from https://www.aplos.com/aws/settings/api/configure                                                         #
#     This should result in a file with the name (aplos_id.key).                                                                     #
#     Put that file in the traditional ./data/ directory below the R code                                                            #
#                                                                                                                                    #
#     Update the api_id value with your api_key value.                                                                               #
#     Set the R working_directory appropriately                                                                                      #
#                                                                                                                                    #
#      Version 1.00                                                                                                                  #
#                                                                                                                                    #
######################################################################################################################################

######################################################################################################################################
#                                                                                                                                    #
# GetAplosAPI                                                                                                                        #
#                                                                                                                                    #
#        Helper function to do the actual API GET and process it down to a usable JSON response.                                     #
#                                                                                                                                    #
# Input: aplos_api_base_url                                                                                                          #
#        aplos_api_url                                                                                                               #
#        api_access_token                                                                                                            #
#                                                                                                                                    #
# Output: response                                                                                                                   #
#                                                                                                                                    #
######################################################################################################################################

GetAplosAPI <- function(aplos_api_base_url, aplos_api_url, api_access_token) {
  
  rawResponse = GET(paste(aplos_api_base_url, aplos_api_url, sep=""),
                    add_headers(Authorization = paste("Bearer: ", api_access_token, sep="")))
  
  charResponse = rawToChar(rawResponse$content)
  response = fromJSON(charResponse)
  if (!is.null(response$exception$code)) {return(paste("ERROR: ", response$exception$code, ":", response$exception$message, sep=""))}
  return(response)
}

######################################################################################################################################

library(httr)
library(PKI)
library(jsonlite)
library(openssl)

# Setup
aplos_api_base_url = "https://www.aplos.com/hermes"
aplos_api_id = AplosID

setwd(    working_directory    )
filePath = "./data/"



# Get the encrypted APlos token
rawResponse = GET(paste(aplos_api_base_url, "/api/v1/auth/", aplos_api_id, sep=""))
charResponse = rawToChar(rawResponse$content)
response = fromJSON(charResponse)

encryptedToken = response$data$token
b64_encryptedToken = base64_decode(encryptedToken)

# Get the private key

fileName <- paste(filePath, aplos_api_id, ".key", sep="")
privateKeyRaw = readChar(fileName, file.info(fileName)$size)

PEM = "-----BEGIN PRIVATE KEY-----"
for (i in seq(1,nchar(privateKeyRaw), 64)) {
  PEM = append(PEM, substr(privateKeyRaw, i, i+63))
}
PEM = append(PEM, "-----END PRIVATE KEY-----")
PEMkey = PKI.load.key(PEM)

# Decrypt the token
api_access_token = rawToChar(PKI.decrypt(b64_encryptedToken, PEMkey, cipher = NULL, iv = NULL))



# Get some Transactions
response = GetAplosAPI(aplos_api_base_url, "/api/v1/transactions", api_access_token)
transactions = as.data.frame(response$data$transactions)
transactions = do.call(data.frame, transactions)

# Get one specific Transaction
aplos_api_url = paste("/api/v1/transactions/", as.character(transactions$id[1]), sep="")
response = GetAplosAPI(aplos_api_base_url, aplos_api_url, api_access_token)
transaction = as.data.frame(response$data$transaction$lines)
transaction = do.call(data.frame, transaction)



# Get some Accounts
response = GetAplosAPI(aplos_api_base_url, "/api/v1/accounts", api_access_token)
accounts = as.data.frame(response$data$accounts)
accounts = do.call(data.frame, accounts)



# Get some Purposes
response = GetAplosAPI(aplos_api_base_url, "/api/v1/purposes", api_access_token)
purposes = as.data.frame(response$data$purposes)
purposes = do.call(data.frame, purposes)



# Get some Contacts
response = GetAplosAPI(aplos_api_base_url, "/api/v1/contacts", api_access_token)
contacts = as.data.frame(response$data$contacts)
contacts = do.call(data.frame, contacts)



# Get some Funds
response = GetAplosAPI(aplos_api_base_url, "/api/v1/funds", api_access_token)
funds    = as.data.frame(response$data$funds)
funds    = do.call(data.frame, funds)
