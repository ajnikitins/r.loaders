
# r.loaders

## Apraksts

r.loaders ir pakotne ar funkcijām, kas atvieglo datu ieguvi no ECB SDW un SQL serveriem.
 
- `get_sdw()` – datu ieguvei no ECB datu noliktavām
	-  Publiskais [ECB datu portāls](https://data.ecb.europa.eu/)
- `get_conn()` – SQL savienojuma izveidei
	- Tālāk var izmantot `DBI` vai `dbplyr` utt., lai veiktu vaicājumus

## Instalācija

```r
#install.packages("remotes")
remotes::install_github("ajnikitins/r.loaders")
```

## SDW vaicājumu saglabāšana kešatmiņā

`get_sdw()` veiktos vaicājumus ir iespējams saglabāt kešatmiņā, kas atvieglo darbu ar apjomīgām datu kopām it īpaši automatizētos un regulāri atjaunotos projektos. Atkārtojot saglabātus vaicājumus, dati tiek lejupielādēti tikai tad, ja SDW publicētie dati ir mainījušies kopš pēdējās saglabāšanas reizes. Tā rezultātā atkarībā no datu apjoma var tikt būtiski ietaupīts laiks, piemēram, regulāri ģenerējot automatizētus slaidus ar atšķirīgiem izmantoto datu atjaunošanas grafikiem.

Pēc noklusējuma `get_sdw()` saglabā veiktos vaicājumus konkrētās R sesijas pagaidu mapē, bet ir iespējams noteikt konkrētu kešatmiņas direktoriju ar `cache_dir` argumentu atsevišķiem vaicājumiem vai ar `loaders.sdw_cache_dir` opciju sesijas ietvaros. Tādējādi kešatmiņu var lietot pastāvīgi.

Kešatmiņas direktoriju var nodzēst ar `clean_sdw_cache()`.

Par plašāku kešatmiņas konfigurāciju var lasīt `get_sdw()` dokumentācijā.

## Pateicības

- Artūrs Jānis Ņikitins (pakotnes autors)
- Latvijas Bankas kolēģiem
- Eric Persson ([`ecb`](https://github.com/expersso/ecb) pakotne)
