# API

The API consists of functions that mirror the functionality of the iPTMNet rest api.

## search
Search the iptmnet database for the given search term.

#### Usage
``` r
search(search_term = "term",term_type=TermType()$TYPE,Role()$ROLE,ptm_vector=c(),organism_vector=c())
```

#### Arguments
| Name | Description |
|-|-|
| __search_term__| The term that you want to search in the database |
|__term_type__| The type of the term that is being searched. It can be `All`,`UniprotID`,`Protein/Gene Name`,`PMID` |
|__role__| The role to be filtered upon. It can be `Enzyme or Substrate`,`Enzyme`,`Substrate`,`Enzyme and Substrate` |
|__ptm_vector__| A vector containing ptm types to filter upon. It can contain following values `Acetylation`,`C-Glycosylation`,`Myristoylation`,`Ubiquitination`,`N-Glycosylation`,`S-Glycosylation`,`Phosphorylation`,`S-Nitrosylation`,`O-Glycosylation`,`Methylation`,`Sumoylation`. Passing an empty vector will match against all possible values. |
|__organism_vector__| A vector containing taxon codes for orgaism to filter upon. |

#### Example
``` r
search("Smad2",TermType()$ProteinOrGeneName,Role()$EnzymeOrSubstrate)
```

#### Output
iptm_id | protein_name | gene_name | synonyms | organism_taxon_code | organism_species | organism_common_name | substrate_role | substrate_num | enzyme_role | enzyme_num | ptm_dependent_ppi_role | ptm_dep_ppi_num | sites | isoforms
--- | --- | ---  | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- 
O70436 | Mothers against decapentaplegic homolog 2; | Smad2 | Madh2 | 10116 | Rattus norvegicus | Rat | True | 0 | False | 0 | False | 0 | 6 | 0
Q1W668 | Mothers against decapentaplegic homolog 2; | SMAD2 |  | 9913 | Bos taurus | Bovine | True | 0 | False | 0 | False | 0 | 4 | 0

## get_info
Get information for the given iptmnet_id

#### Usage
``` r
get_info(id = "iptmnet_id")
```

#### Arguments
| Name | Description |
|-|-|
| __id__| iPTMnet ID |

#### Example
``` r
get_info("Q15697")
```

#### Output
``` json
{
  "uniprot_ac": "Q15796",
  "uniprot_id": "SMAD2_HUMAN",
  "protein_name": "Mothers against decapentaplegic homolog 2;",
  "gene_name": "SMAD2",
  "synonyms": [
    "MADH2",
    "MADR2"
  ],
  "organism": {
    "taxon_code": 9606,
    "species": "Homo sapiens",
    "common_name": "Human"
  },
  "pro": {
    "id": "PR:Q15796",
    "name": "mothers against decapentaplegic homolog 2 (human)",
    "definition": "A smad2 that is encoded in the genome of human.",
    "short_label": "hSMAD2",
    "category": "organism-gene"
  }
}
```


## get_msa
`Not implemented yet`

## get_proteoforms
Get proteoforms for the given iptmnet_id

#### Usage
``` r
get_proteoforms(id="iptmnet_id")
```

#### Arguments
| Name | Description |
|-|-|
| __id__| iPTMnet ID |

#### Example
``` r
get_proteoforms("Q15796")
```

#### Output
pro_id | label | sites | ptm_enzyme_id | ptm_enzyme_label | source |
--- | --- | --- | --- | --- | --- |
PR:000025934 | hSMAD2/iso:1/Phos:1 | pS465,pS467 | PR:000025963 | transforming growth factor beta type II receptor homodimeric complex phosphorylated form (human) | PRO |
PR:000025935 | hSMAD2/iso:1/Phos:2 | pS465,pS467 | PR:000025963 | transforming growth factor beta type II receptor homodimeric complex phosphorylated form (human) | PRO |
PR:000025935 | hSMAD2/iso:1/Phos:2 | pT8,pT220,pS245,pS250,pS255 | PR:000026189 | hMAPK3/iso:1/Phos:1 | PRO |


## get_ptm_dependent_ppi
Get Post translational modification(PTM) dependent Protein-Protein interactions for the given iptmnet_id

#### Usage
``` r
get_ptm_dependent_ppi(id="iptmnet_id")
```

#### Arguments
| Name | Description |
|-|-|
| __id__| iPTMnet ID |

#### Example
``` r
get_ptm_dependent_ppi("Q15796")
```

#### Output
ptm_type | substrate_uniprot_id | substrate_name | site | interactant_uniprot_id | interactant_name | association_type | source | pmid
--- | --- | --- | --- | --- | --- | --- | --- | --- |
Phosphorylation | P49841 | GSK3B | S9 | Q15796 | SMAD2 | increased_association | eFIP | 21996745 | 
Phosphorylation | Q15796 | SMAD2 | S467 | Q13485 | SMAD4 | association | eFIP | 9346908 | 

## get_ppi_for_proteoforms
Get Protein-Protein interactions along with corresponding proteoforms for the given iPTMnet ID

#### Usage
``` r
get_ppi_for_proteoforms(id="iptmnet_id")
```

#### Arguments
| Name | Description |
|-|-|
| __id__| iPTMnet ID |

#### Example
``` r
get_ppi_for_proteoforms("Q15796")
```

#### Output
protein_1_pro_id | protein_1_label | relation | protein_2_pro_id | protein_2_label | source | pmids
--- | --- | --- | --- | --- | --- | --- | --- |
PR:000025934 | hSMAD2/iso:1/Phos:1 | Interaction | PR:Q13485 | hSMAD4 | PRO | 9311995 
PR:000025935 | hSMAD2/iso:1/Phos:2 | Interaction | PR:Q13485 | hSMAD4 | PRO | 12193595
PR:000045371 | hSMAD2/iso:1/UnPhos:1 | Interaction | |  | PRO | 8980228


## get_ptm_enzymes_from_list
Get PTM enzymes for a list of sites.


#### Usage
``` r
get_ptm_enzymes_from_list(sites=list())
```

#### Arguments
| Name | Description |
|-|-|
| __sites__| list of sites. Each site should have the following attributes: `substrate_ac`, `site_residue`, `site_position` |

#### Example
``` r
# create a list of sites
sites = list(
    list(
      substrate_ac="Q15796",
      site_residue="K",
      site_position="19"
    ),
    list(
      substrate_ac="Q15796",
      site_residue="T",
      site_position="8"
    ),
    list(
      substrate_ac="P04637",
      site_residue="K",
      site_position="120"
    )
  )
  
# perform the query
get_ptm_enzymes_from_list(sites)
```

#### Output
ptm_type | site | site_position | score | source | pmid | enz_name | enz_id | sub_name | sub_id |
--- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
Acetylation | K120 | 120 | 2 | UniProt | 23431171 | KAT6A | Q92794 | TP53 | P04637 |
Phosphorylation | T8 | 8 | 3 | nextProt | 19201832,15241418 | CDK2 | P24941 | SMAD2 | Q15796 | 


## get_ptm_enzymes_from_file
Get PTM enzymes for sites in the given csv file


#### Usage
``` r
get_ptm_enzymes_from_file(file_name='/path/to/file.csv')
```

#### Arguments
| Name | Description |
|-|-|
| __file_name__| Path to the csv file containing the list of sites . First line should be a header with following attributes: `substrate_ac`, `site_residue`, `site_position` |

#### Example
``` r

# perform the query
get_ptm_enzymes_from_file("sites.csv")
```

#### Output
ptm_type | site | site_position | score | source | pmid | enz_name | enz_id | sub_name | sub_id |
--- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
Acetylation | K120 | 120 | 2 | UniProt | 23431171 | KAT6A | Q92794 | TP53 | P04637 |
Phosphorylation | T8 | 8 | 3 | nextProt | 19201832,15241418 | CDK2 | P24941 | SMAD2 | Q15796 | 

## get_ptm_ppi_from_list
Get Post translational modification(PTM) dependent Protein-Protein interactions(PPI) for the given list of site


#### Usage
``` r
get_ptm_ppi_from_list(sites=list())
```

#### Arguments
| Name | Description |
|-|-|
| __sites__| list of sites. Each site should have the following attributes: `substrate_ac`, `site_residue`, `site_position` |

#### Example
``` r
# create a list of sites
sites = list(
    list(
      substrate_ac="Q15796",
      site_residue="K",
      site_position="19"
    ),
    list(
      substrate_ac="Q15796",
      site_residue="T",
      site_position="8"
    ),
    list(
      substrate_ac="P04637",
      site_residue="K",
      site_position="120"
    )
  )
  
# perform the query
get_ptm_ppi_from_list(sites)
```

#### Output
ptm_type | site | site_position | association_type | source | pmids | interactant_id | interactant_name |
--- | --- | --- | --- | --- | --- | --- | --- |
Phosphorylation | S378 | 378 | association | rlimsp | 27462439 | Q92540 | SMG7 |
Phosphorylation | S378 | 378 | increased_association | rlimsp | 22911849 | P08047 | SP1 |


## get_ptm_ppi_from_file
Get Post translational modification(PTM) dependent Protein-Protein interactions(PPI) for sites in the given csv


#### Usage
``` r
get_ptm_ppi_from_file(file_name="/path/to/file.csv")
```

#### Arguments
| Name | Description |
|-|-|
| __file_name__| Path to the csv file containing the list of sites . First line should be a header with following attributes: `substrate_ac`, `site_residue`, `site_position` |

#### Example
``` r
# perform the query
get_ptm_ppi_from_file("sites.csv")
```

#### Output
ptm_type | site | site_position | association_type | source | pmids | interactant_id | interactant_name |
--- | --- | --- | --- | --- | --- | --- | --- |
Phosphorylation | S378 | 378 | association | rlimsp | 27462439 | Q92540 | SMG7 |
Phosphorylation | S378 | 378 | increased_association | rlimsp | 22911849 | P08047 | SP1 |


## set_host_url
Set the URL of the iPTMnet API server. By default the client points to the public iPTMnet api server located at - `https://annotation.dbi.udel.edu/iptmnet/api`

#### Usage
``` r
set_host_url(url="http://localhost.com")
```

#### Arguments
| Name | Description |
|-|-|
| __url__| URL of the iPTMnet api server|


