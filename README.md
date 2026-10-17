# Workshop: Tvorba kontinuálních norem při vývoji psychodiagnostických metod
Zde najdete materiály a podklady pro prekonferenční workshop, který se konal 22. října 2026 na Fakultě sociálních studií Masarykovy univerzity v rámci konference [Psychologická diagnostika Brno 2026](https://psychodiagnostika.fss.muni.cz/). Lektorem byl [Hynek Cígler](https://www.muni.cz/lide/175803-hynek-cigler).

## Anotace workshopu
Tvorba testových norem patří k nejméně viditelným, přesto však zcela kritickým krokům vývoje psychodiagnostické metody určené pro praktické použití. Kvalita norem zásadně ovlivňuje validitu interpretace testových skórů. V posledních dekádách přitom došlo v oblasti normování k významnému metodologickému vývoji, který běžné učebnice psychometriky a psychologické diagnostiky reflektují jen omezeně. Důsledkem je, že uživatelé testů kontinuální normy obvykle nevyžadují a autoři či distributoři diagnostických metod je jen zřídka systematicky poskytují.

Kontinuální normy, respektive vertikální a horizontální vyhlazování normativních odhadů, nabízejí významné výhody zejména v situacích, kdy je testový výkon závislý na spojitém prediktoru, typicky na věku. Namísto tvorby separátních norem pro diskrétně vymezené věkové kohorty, například pro šestileté, sedmileté až dvacetileté respondenty, lze normativní vztahy odhadovat nad celým standardizačním souborem současně. Tento postup umožňuje efektivněji využít dostupná data, snížit výběrovou chybu a získat stabilnější normativní odhady i při menších vzorcích. Vedlejším přínosem je také přesnější věkové zacílení norem: namísto normativní tabulky s přesností na rok či půlrok lze odhadovat normativní výkony s přesností na měsíc či libovolný jiný úsek limitovaný pouze smysluplností a výběrovou chybou. Praktickým důsledkem je proto nejen validnější interpretace testových skórů v psychologické praxi, ale také významná úspora pro autory a distributory diagnostických metod. Požadavky na velikost standardizačních vzorků totiž mohou být při vhodném modelování nižší než u tradičních tabulkových norem.

Úvodní část workshopu představí základní teoretická východiska tvorby testových norem obecně a následně se zaměří na principy jejich vyhlazování. Pozornost bude věnována různým metodám kontinuálního normování, zejména postupům založeným na Taylorových polynomech, dále mediánovému rozdělení s parametry vyhlazenými pomocí loess regrese i méně vhodnému přístupu založenému na rezidualizovaných regresních normách, včetně jeho rizik a nevýhod. Zmíněny budou také spojité postupy pro normalizaci testových skórů, například McCallova plošná transformace s jádrovým vyhlazením (kernel smoothing). U všech metod budou zdůrazněny jejich předpoklady včetně náročnosti na velikost vzorku, důsledky nedostatečného vzorku a porušení reprezentativity. Workshop otevře také související téma spojitého odhadu chyby měření a reliability s využitím explanační teorie zobecnitelnosti, využitelné zejména při pilotážích testů s malými vzorky a výkonem silně závislým na věku, a metody spojité položkové analýzy kontrolující věk participantů.

Druhá část workshopu bude prakticky orientovaná. Účastníci si na ukázkových datech vyzkoušejí základní kroky tvorby kontinuálních norem v prostředí R, zejména s využitím balíčku cNORM. Praktické demonstrace budou vycházet z dat reálných psychologických testů, především z nedávného standardizačního projektu Testů školních dovedností (BACH). Workshop účastníkům poskytne základní teoretickou orientaci i praktické dovednosti potřebné pro tvorbu kontinuálních norem v běžných situacích. Součástí bude také doporučení další literatury a zdrojů pro osvojení pokročilejších postupů.

## Pokyny

### Seznam materiálů v tomto úložišti
- **workshop.R** - hlavní podklad pro workshop
- data.csv - cvičný dataset se skóry vybraných Testů školních dovedností (BACH)
- ilustrace.R - simulace použité v prezentaci
- cnorm_workshop.Rproj - nastavení R projektu

Součástí úložiště jsou další soubory, které nejsou určené k veřejnému použití, zejména:
- helper.R - soubor, který vyřezává vybraná data z matice baterie BACH.

### Popis dat
Cvičná data reprezentují vybrané subtesty baterie BACH. Data obsahují tyto proměnné:
- vek_roky   - věk respondenta v letech
- group      - four age groups
- vek_kategorie - more age groups
- T01_M      - rychlost práce (M-skór) v testu Grafomotorické tempo
- T01_IQ     - standardní IQ skór pro T01
- M06_W      - matematické usuzování (Slovní úlohy) - W-skór
- M06_IQ     - std IQ skór pro M06_W
- C07_M      - rychlost práce v subtestů Diktát (M-skór)
- C07_IQ_cas - příslušný std IQ skór
- C07_W      - správnost práce v subtestu Diktát (W-skór)
- C07_IQ_spravnost - příslušný std IQ skór

### Jak začít
Stáhněte si veškeré soubory do libovolné složky na vašem počítači a spusťte soubor cnorm_workshop.Rproj, následně pracujte s souborem workshop.R. Výsledkem budou vaše první kontinuální normy!
