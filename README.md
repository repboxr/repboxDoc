In repbox I will refer to *documents* mainly as the article and potential online appendices. This package contains tools and heuristics to prepare such documents.

Different types of documents and heuristically extracted information will be stored in the `doc` folder. For an article, that exists in a pdf and html version and has two documents with online appendices, the folder structure would be:

```
doc\
  \art_pdf
  \art_html
  \app1_pdf
  \app2_pdf
```

- When finished `repboxDoc` will take a lot of functionality that perviously was in `repboxArt`. Main difference is that it most tools first work indepedently on the different doc folders. Possibly when everything is finished `repboxArt` may become almost obsolete.

- Systematic extraction of information, tables, mapping, will later be part of the *fuzzy production* framework currently implemented in `repboxAI` and `FuzzyProduction`. `repboxDoc` mainly creates initial document preparation and has functions that can be called by `repboxAI`.

- Heuristic table extraction mainly relies on `repboxTableTools`. 
