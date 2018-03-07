# API

The API consists of functions that mirror the functionality of the iPTMNet rest api.

## search
Search the iptmnet database for the given search term.

### Usage
``` R
search(search_term = "term",term_type=TermType()$TYPE,Role()$ROLE,ptm_vector=c(),organism_vector=c())
```

### Arguments
| Name | Description |
|-|-|
| __search_term__| The term that you want to search in the database |
|__term_type__| The type of the term that is being searched. It can be `All`,`UniprotID`,`Protein/Gene Name`,`PMID` |
|__role__| The role to be filtered upon. It can be `Enzyme or Substrate`,`Enzyme`,`Substrate`,`Enzyme and Substrate` |
|__ptm_vector__| A vector containing ptm types to filter upon. It can contain following values `Acetylation`,`C-Glycosylation`,`Myristoylation`,`Ubiquitination`,`N-Glycosylation`,`S-Glycosylation`,`Phosphorylation`,`S-Nitrosylation`,`O-Glycosylation`,`Methylation`,`Sumoylation`. Passing an empty vector will match against all possible values. |
|__organism_vector__| A vector containing taxon codes for orgaism to filter upon. |

### Output
iptm_id | protein_name | gene_name | synonyms | organism_taxon_code | organism_species | organism_common_name | substrate_role | substrate_num | enzyme_role | enzyme_num | ptm_dependent_ppi_role | ptm_dep_ppi_num | sites | isoforms
--- | --- | ---  | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- 
O70436 | Mothers against decapentaplegic homolog 2; | Smad2 | Madh2 | 10116 | Rattus norvegicus | Rat | True | 0 | False | 0 | False | 0 | 6 | 0
Q1W668 | Mothers against decapentaplegic homolog 2; | SMAD2 |  | 9913 | Bos taurus | Bovine | True | 0 | False | 0 | False | 0 | 4 | 0

## get_info

## get_msa

## get_proteoforms

## get_ptm_dependent_ppi

## get_ppi_for_proteoforms

## get_ptm_enzymes_from_list

## get_ptm_enzymes_from_file

## get_ptm_ppi_from_list

## get_ptm_ppi_from_file

## seT_host_url



