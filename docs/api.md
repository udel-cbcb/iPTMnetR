# API

The API consists of functions that mirror the functionality of the iPTMNet rest api.

## search
Search the iptmnet database for the given search term.

### Usage
``` r
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

### Example
``` r
search("Smad2",TermType()$ProteinOrGeneName,Role()$EnzymeOrSubstrate)
```

### Output
iptm_id | protein_name | gene_name | synonyms | organism_taxon_code | organism_species | organism_common_name | substrate_role | substrate_num | enzyme_role | enzyme_num | ptm_dependent_ppi_role | ptm_dep_ppi_num | sites | isoforms
--- | --- | ---  | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- 
O70436 | Mothers against decapentaplegic homolog 2; | Smad2 | Madh2 | 10116 | Rattus norvegicus | Rat | True | 0 | False | 0 | False | 0 | 6 | 0
Q1W668 | Mothers against decapentaplegic homolog 2; | SMAD2 |  | 9913 | Bos taurus | Bovine | True | 0 | False | 0 | False | 0 | 4 | 0

## get_info
Get information for the given iptmnet_id

### Usage
``` r
get_info(id = "iptmnet_id")
```

### Arguments
| Name | Description |
|-|-|
| __id__| iPTMnet ID |

### Example
``` r
get_info("Q15697")
```

### Output
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
Not implemented yet

## get_proteoforms
Get proteoforms for the given iptmnet_id

### Usage
``` r
get_proteoforms(id="iptmnet_id")
```

### Arguments
| Name | Description |
|-|-|
| __search_term__| The term that you want to search in the database |
|__term_type__| The type of the term that is being searched. It can be `All`,`UniprotID`,`Protein/Gene Name`,`PMID` |
|__role__| The role to be filtered upon. It can be `Enzyme or Substrate`,`Enzyme`,`Substrate`,`Enzyme and Substrate` |
|__ptm_vector__| A vector containing ptm types to filter upon. It can contain following values `Acetylation`,`C-Glycosylation`,`Myristoylation`,`Ubiquitination`,`N-Glycosylation`,`S-Glycosylation`,`Phosphorylation`,`S-Nitrosylation`,`O-Glycosylation`,`Methylation`,`Sumoylation`. Passing an empty vector will match against all possible values. |
|__organism_vector__| A vector containing taxon codes for orgaism to filter upon. |

### Example
``` r
get_proteoforms("Q15796")
```

### Output
pro_id | label | sites | ptm_enzyme_id | ptm_enzyme_label | source |
--- | --- | --- | --- | --- | --- |
PR:000025934 | hSMAD2/iso:1/Phos:1 | pS465,pS467 | PR:000025963 | transforming growth factor beta type II receptor homodimeric complex phosphorylated form (human) | PRO |
PR:000025935 | hSMAD2/iso:1/Phos:2 | pS465,pS467 | PR:000025963 | transforming growth factor beta type II receptor homodimeric complex phosphorylated form (human) | PRO |
PR:000025935 | hSMAD2/iso:1/Phos:2 | pT8,pT220,pS245,pS250,pS255 | PR:000026189 | hMAPK3/iso:1/Phos:1 | PRO |


## get_ptm_dependent_ppi

## get_ppi_for_proteoforms

## get_ptm_enzymes_from_list

## get_ptm_enzymes_from_file

## get_ptm_ppi_from_list

## get_ptm_ppi_from_file

## seT_host_url



