#' Set of Graydon company colors
#'
#' A vector containing 9 Graydon company colors in hexadecimal format.
#'
"col_graydon"

#' Low value color
#'
#' A variable containing a Graydon company color used for the lowest value in positive
#' valued gradients. The color value is in hexadecimal format.
#'
"col_graydon_low"

#' High value color
#'
#' A variable containing a Graydon company color used for the highest value in positive
#' valued gradients. The color value is in hexadecimal format.
#'
"col_graydon_high"

#' Map outlining the Dutch country borders
#'
#' \url{https://en.wikipedia.org/wiki/Netherlands}
#'
"sp_nl_country"

#' Map outlining the Dutch province borders
#'
#' \url{https://en.wikipedia.org/wiki/Provinces_of_the_Netherlands}
#'
"sp_nl_province"

#' Map outlining the Dutch COROP borders
#'
#' \url{https://en.wikipedia.org/wiki/COROP}
#'
"sp_nl_corop"

#' Map outlining the Dutch manucipality borders
#'
#'
"sp_nl_gemeenten"

#' Map outlining the Dutch postal code borders
#'
#' \url{https://en.wikipedia.org/wiki/Postal_codes_in_the_Netherlands}
#'
"sp_nl_postcode"

#' Map outlining the Belgium country borders
#'
#' \url{https://en.wikipedia.org/wiki/Belgium}
#'
"sp_be_country"

#' Map outlining the Belgium gewest borders
#'
#' \url{https://en.wikipedia.org/wiki/Communities,_regions_and_language_areas_of_Belgium}
#'
"sp_be_gewest"

#' Map outlining the Belgium province borders
#'
#' \url{https://en.wikipedia.org/wiki/Provinces_of_Belgium}
#'
"sp_be_province"

#' Map outlining the UK state borders
#'
#' \url{https://en.wikipedia.org/wiki/United_Kingdom}
#'
"sp_uk_state"

#' Map outlining the UK country borders
#'
#' \url{https://en.wikipedia.org/wiki/Countries_of_the_United_Kingdom}
#'
"sp_uk_country"

#' Map outlining the UK lieutenancy area borders
#'
#' \url{https://en.wikipedia.org/wiki/Lieutenancy_area}
#'
"sp_uk_lieutenancy"

#' The SBI codes which which are used to describe a companies main economic activity.
#'
#' A dataset correcting character problem of original source
#'
#' \url{https://sbi.cbs.nl/cbs.typeermodule.typeerservicewebapi/content/angular/app/#/}
#'
#' @format A data frame with 1.887 rows and 4 variables (previously 1.455 rows):
#' \describe{
#'   \item{code_SBI}{Code used to identify economic activities}
#'   \item{code_SBI_parent}{Code of the economic activity description that is more general}
#'   \item{description_SBI}{The Dutch description of the economic activity}
#'   \item{hierarchy layer}{The hierarchical position of the code in the total economic activity tree;
#'   the higher the number, the more specific the code is.}
#' }
"tbl_SBI"

#' DO NOT USE THIS DATA FRAME. PLEASE USE tbl_SBI
#'
#' This data frame is used as a backup.
#'
"tbl_SBI_new"

#' The 2008 NACE codes which which are used to describe a companies main economic activity.
#'
#' \url{https://ec.europa.eu/eurostat/documents/3859598/5902521/KS-RA-07-015-EN.PDF}
#'
#' @format A data frame with 1.455 rows and 4 variables:
#' \describe{
#'   \item{code_NACE}{Code used to identify economic activities}
#'   \item{code_NACE_parent}{Code of the economic activity description that is more general}
#'   \item{description_NACE_nl}{The Dutch description of the economic activity}
#'   \item{description_NACE_fr}{The French description of the economic activity}
#'   \item{description_NACE_de}{The German description of the economic activity}
#'   \item{description_NACE_en}{The English description of the economic activity}
#'   \item{hierarchy layer}{The hierarchical position of the code in the total economic activity tree;
#'   the higher the number, the more specific the code is.}
#' }
"tbl_NACE"


#' The postcode table for The Netherlands
#'
#' \url{https://ec.europa.eu/eurostat/documents/3859598/5902521/KS-RA-07-015-EN.PDF}
#'
#' @format A data frame with 1.455 rows and 4 variables:
#' \describe{
#'   \item{postcode_4}{4 digit postcode}
#'   \item{town}{Town}
#'   \item{municipality}{Municipality}
#'   \item{province}{Province}
#'   \item{code_telephone_area}{Telephone area code}
#'   \item{lat}{Latitude}
#'   \item{lon}{Longitude}
#'   \item{type_of_address}{Type of postcode use}
#' }
"tbl_postcode_4_nl"

#' A sample of 1.835 UK companies
#'
#' A dataset containing the first few companies from our UK GDI dataset
#'
#' @format A data frame with 1.835 rows and 23 variables:
#' \describe{
#'   \item{GDB_ORG_YUID}{Company identifier}
#'   \item{GB_CRO_REGISTRATION_NUMBER}{Chamber of commerce identifier}
#'   \item{GB_Y_COMPANY_ID}{Legacy company identifier}
#'   \item{PRINCIPAL_NAME}{Legacy company identifier}
#'   \item{ADDRESS_LINE_1_RA}{Address line for the registred address}
#'   \item{ADDRESS_LINE_2_RA}{Address line for the registred address}
#'   \item{ADDRESS_LINE_3_RA}{Address line for the registred address}
#'   \item{ADDRESS_LINE_4_RA}{Address line for the registred address}
#'   \item{ADDRESS_LINE_5_RA}{Address line for the registred address}
#'   \item{POSTCODE_RA}{Legacy company identifier}
#'   \item{POSTCODE_RA}{Legacy company identifier}
#' }
"tbl_companies_uk"

#' The SBI codes which which are used to describe a companies main economic activity and the number of companies in nl
#'
#' \url{https://sbi.cbs.nl/cbs.typeermodule.typeerservicewebapi/content/angular/app/#/}
#'
#' @format A data frame with 1.455 rows and 5 variables:
#' \describe{
#'   \item{code_SBI}{Code used to identify economic activities}
#'   \item{code_SBI_parent}{Code of the economic activity description that is more general}
#'   \item{description_SBI}{The Dutch description of the economic activity}
#'   \item{hierarchy_layer}{The hierarchical position of the code in the total economic activity tree;
#'   the higher the number, the more specific the code is.}
#'   \item{qty_companies}{The number of companies in The Netherlands}
#' }
"tbl_SBI_count"


#' Relations between companies
#'
#' @format A data frame with 392.835 rows and 2 variables:
#' \describe{
#'   \item{id_company}{Code used to identify economic activities}
#'   \item{id_company_parent}{Code of the economic activity description that is more general}
#' }
"tbl_company_relations"
