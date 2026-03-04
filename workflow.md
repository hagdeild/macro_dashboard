# Workflow - Macro Dashboard

## Manual monthly downloads

These Excel files must be downloaded manually and placed in the correct location.
Update these when Sedlabanki publishes new data (typically monthly or quarterly).

### From Hagvisar Sedlabankans
Source: https://sedlabanki.is/frettir-og-utgefid-efni/rit-og-skyrslur/

| File | Location | Used in | Section |
|------|----------|---------|---------|
| HV_Tolur_i_myndir_V_Opinber_fjarmal.xlsx | data/hagvisar/ | get_data.R | 2.6.0 Skuldir rikisins |
| HV_Tolur_i_myndir_II_Framleidsla_og_eftirspurn.xlsx | data/hagvisar/ | get_data.R | 9.6.0 Vaentingar fyrirtaekja, 9.8.1 Sementssala |

### From Peningamal Sedlabankans
Source: https://sedlabanki.is/frettir-og-utgefid-efni/rit-og-skyrslur/

| File | Location | Used in | Section |
|------|----------|---------|---------|
| Verdbolguvaentingar-a-mismunandi-maelikvarda.xlsx | data/expectations/ | get_data.R | 2.4.0 Verdbolguvaentingar (heimili, fyrirtaeki, breakeven) |
| Vaentingar_markadsadila.xlsx | data/expectations/ | get_data.R | 2.4.3 Markadsadilar |

### From Greidslujofnudur
Source: https://sedlabanki.is/gagnatorg/greidslujofnudur-vid-utlond/

| File | Location | Used in | Section |
|------|----------|---------|---------|
| Greidslujofnudur.xlsx | data/ | get_data.R | 2.2.0 Vidskiptajofnudur |

### From Erlend stada thjodarbusins
Source: https://sedlabanki.is/gagnatorg/erlend-stada-thjodarbusins/

| File | Location | Used in | Section |
|------|----------|---------|---------|
| ErlendStadaThjodarbusinsISL.xlsx | data/ | get_data.R | 2.5.0 Erlend stada |

### QMM dataset
Source: Sedlabanki QMM publication (quarterly)

| File | Location | Used in | Section |
|------|----------|---------|---------|
| qmm.xlsx | data/ | get_data.R | 1.1.3 QMM, vnv_qrt (via cpi), 3.2.7 Framleidni |

### From Greidslumidlun
Source: https://sedlabanki.is/gagnatorg/greidslumidlun/

| File | Location | Used in | Section |
|------|----------|---------|---------|
| kortavelta.xlsx | data/raw/ | get_data.R | 7.3.0 Kortavelta |

---

## Automatic - Daily (macro_data_update.R)

These run via task scheduler and must run daily (business days).

| Data | Source | Method | Storage |
|------|--------|--------|---------|
| KRAFA (yield curve) | lanamal.is | Web scrape + Nelson-Siegel | Daily files in data/lanamal/, monthly average written to data/krafa.csv on last day of month |
| Gengisvisitala | sedlabanki.is/gagnatorg | Chromote scrape | Appended to data/gengisvisitala.csv |

---

## Automatic - On each get_data.R run

These are pulled automatically from APIs or scraped every time get_data.R runs.

### Hagstofa APIs (px.hagstofa.is)
| Data | Section |
|------|---------|
| Mannfjoldi (quarterly) | 1.1.1 |
| Mannfjoldi a vinnufaerum aldri (yearly) | 1.1.2 |
| Visitala neysluverds (monthly + yearly) | 1.1.4 |
| VLF a nafnvirdi (quarterly + yearly) | 1.1.5 |
| GDP components - kedjutengt (quarterly) | 2.0.0 |
| Undirliggjandi verdbolga (monthly) | 2.3.0 |
| Starfandi (monthly) | 3.1.0 |
| Laus storf (quarterly) | 3.2.5 |
| Vinnulitlir (quarterly) | 3.2.6 |
| Hlutfall erlendra starfandi (monthly) | 3.2.8 |
| Vinnustundir (monthly) | 3.2.8 |
| Launahlutfall (yearly) | 3.2.9 |
| Launavisitala (monthly) | 4.1.0 |
| Laun eftir starfsstett (monthly) | 4.2.0 |
| Laun eftir launthegahopum (monthly) | 4.3.0 |
| Laun eftir atvinnugrein (monthly) | 4.4.0 |
| Radstofunartekjur - tekjuskiptingaruppgjor (quarterly) | 4.5.1 |
| Radstofunartekjur - skattagogn (yearly) | 4.5.2 |
| Launadreifing (yearly) | 4.6.0 |
| Ferdamenn (monthly) | 7.1.0 |
| Gistinaetur (monthly) | 7.2.0 |
| Aldurspyramidi (yearly) | 8.1.0 |
| Adfluttir/brottfluttir (quarterly) | 8.2.0 |
| Tekjuskiptingaruppgjor fyrirtaekja (yearly) | 9.1.0 |
| Gjaldthrot fyrirtaekja (monthly) | 9.3.0 |
| Nyskraningar fyrirtaekja (monthly) | 9.4.0 |

### Web scrapes (on get_data.R run)
| Data | Source | Section | Also writes to |
|------|--------|---------|----------------|
| VMST (atvinnuleysi, lengd) | island.is Excel download | 3.0.1, 3.2.1-3.2.3 | (in memory only) |
| Skuldabref | lanamal.is scrape | 6.2.0 | data/skuldabref.csv (appended) |
| Styrivextir | cbrates.com scrape | 6.3.0 | data/styrivextir.csv (appended) |
| MyIgloo leigulistingar | myigloo.is Chromote scrape | 5.4.0 | data/myigloo.csv (overwritten) |

### External APIs (on get_data.R run)
| Data | Source | Section |
|------|--------|---------|
| Leiguverd visitala | HMS (hms.is) CSV | 5.1.0 |
| Kaupverd visitala | HMS (hms.is) CSV | 5.2.0 |
| OMXI15 | FRED | 6.1.1 |
| S&P 500 | Yahoo Finance (tidyquant) | 6.1.2 |
| Hravöruverd (8 series) | FRED | 6.6.0 |

### Read from local CSV (maintained by macro_data_update.R)
| Data | File | Section |
|------|------|---------|
| Krafa (yield curve history) | data/krafa.csv | 6.4.0 |
| Gengisvisitala | data/gengisvisitala.csv | 6.5.0 |
