---
title: "Werkplan pelagische primaire productie Westerschelde"
author: "Willem Stolte"
date: "2022-10-07"
output:
  bookdown::html_document2
---

# Achtergrond

Natuurlijke processen in de Westerschelde staan onder druk door de verschillende gebruiksfuncties, transport en veiligheid. Hierdoor is er een onnatuurlijke verhoudingn van diepe en ondiepe delen en een verhoogde troebelheid, waardoor de pelagische primaire productie sterk lichtgelimiteerd is. Er is daardoor een risico dat de primaire productie niet genoeg is om voldoende schelpdieren te voorzien van voedsel, waardoor doelen voor de vogelrichtlijn niet gehaald kunnen worden.

Voor het afwegen van maatregelen voor veiligheid en transport tegen natuurlijke waarden is het belangrijk om te weten in hoeverre de primaire productie afhangt van de omstandigheden in het estuarium. Licht, maar ook temperatuur zijn belangrijk voor de specifieke productie, de hoeveelheid plankton die zich kan opbouwen hangt vaak ook nog af van de verblijftijd. Deze relaties zijn nog niet bepaald voor de Westerschelde.

Primaire productie wordt daarom ook gemeten in de Westerschelde. Gemeten productie kan dan gerelateerd worden aan omgevingsfactoren om de relaties helderder te krijgen. Dit is tot nu toe nog niet gedaan voor de Westerchelde. Dit onderzoek draagt hieraan bij.

Helaas is de meetreeks voor pelagische primaire productie niet compleet. Tot en met 2014 is de productie gemeten met één methodiek (14C). Voor 2015 zijn geen metingen. Hierna is begonnen een minder consistente meetreeks met een andere methodiek (FRRF). De resultaten daarvan lijken wisselend. Er is geen overlap geweest in tijd voor deze twee methoden in de Westerchelde. Dit onderzoek wil een overzicht geven van deze metingen en waar mogelijk ook schattingen maken van primaire productie voor jaren waar weinig gegevens voorhanden zijn.

Het is mogelijk om primaire productie te schatten op basis van satellietbeelden. Informatie over biomassa, lichtdoordringing en temperatuur wordt gebruikt om een (vlakdekkende) schatting te geven van primaire productie. Grote voordeel is dat deze methode onafhankelijk is van de vorige, en dat deze bij goede condities vlakdekkend is. Het is belangrijk dat er genoeg overvliegdagen zijn met goede condities (helder weer). Ook is de troebelheid van het water in het estuarium en het feit dat delen ondiep zijn (bodemeffect) soms een beperkende factor.

## Doel:

-   leidt criteria af om FRRF metingen te beoordelen en correct te interpreteren.
-   verklaar variatie in PPP met omgevingsfactoren
-   construeer heranalyse PPP aan de hand van relaties met omgevingsvariabelen en informatie uit satellieten.

# Methodiek

## Tijdschalen:

We kunnen op verschillende tijdschalen gaan rekenen.

-   jaarlijkse productie (integraal over metingen in groeiseizoen) - Dit is vrij grof, maar waarschijnlijk bruikbaar genoeg voor een koppeling met hogere trofische niveaus. Soortensamenstelling van fytoplankton kan gebruikt worden om te controleren of er geen grote verschillen zijn in seizoensdynamiek tussen jaren.
-   seizoensvariatie (evt maandelijks) - Het uitsplitsen in seizoenen kan nuttige ecologische informatie opleveren (wat is de productie tijdens de broedval van schelpdieren?). Het is te verwachten dat onzekerheden toenmen bij verkleining van de tijdschaal.

Verder moet er rekening gehouden worden met het feit dat het estuarium een dynamisch gebied is (getijbeweging) en dat MWTL metingen NIET gemiddeld over een getijcyclus worden gedaan (monsters worden rond hoogwater genomen).

## Ruimteschalen

-   gehele Westerschelde + monding
-   deelgebieden, nader te bepalen

## Beschikbare relevante gegevens:

-   PPP, 14C en FRFF
-   MWTL oppwater + zwevendstof
-   metingen in samenhang met PPP metingen (scanfish?)
-   EO
-   Algengroepen

## Werkwijze

Bepaal relatie tussen omgevingsfactoren en gemeten (MWTL) PPP. Gebruik verschillende technieken (GLM, GAM). Pas relatie toe op perioden met ontbrekende metingen.

Bepaal PPP vlakdekkend met behulp van schattingen van spm, chlfa, kd en evt temperatuur uit satellietgegevens. Deze methode is alleen kansrijk voor de laatste 5-7 jaren.

Vergelijk beide methodieken.

Maak inschatting van PPP over de gehele periode met een betrouwbaarheid.

## Planning + Budget

| Tijd         | activiteit              | geschatte kosten | wie            |
|--------------|-------------------------|------------------|----------------|
| Okt 2021     | in situ data preparatie |                  | Willem/Dick    |
| Okt-Nov 2021 | EO data preparatie      |                  | Marieke/Daphne |
| Nov-Dec 2022 | Eerste analyse in situ  |                  | Willem/Dick    |
| Dec          | Setup model EO PP       |                  | Marieke/Daphne |

: planning en budget

Afhankelijk van beschikbaarheid NIOZ (Dick, Daphne) worden werkzaamheden uitbesteed. 

## Reproduceerbaarheid en rapportage

Scripts worden onder versie beheerd op Github (https://github.com/wstolte/ScheldeCC)
Overleg over mapstructuur aub.

# Overige informatie

Losse notities overleg:

-   laatste jaren is FRFF parallel met 14C gemeten voor 1 station (alleen op grens NL en BE)
-   Nicole Dijkman RWS vragen naar FRFF metingen. Jetta Vlaming (UA) vragen voor 14C metingen

over metingen FRFF

-   niet altijd duidelijk wat de kwaliteit is. Mogelijk meer duidelijkheid bij vergelijken chlfa FRFF en fluorescentie (scanfish). Bij grotere afwijking vlaggen.

Daphne: grote verschillen in frequentie van bruikbare EO beelden. Vermoedelijk slechte beschikbaarheid in 2015 en 2016.

EO: Er kan voor de laatste jaren (grofweg vanaf 2015) een EO opgebouwd fysisch model gemaakt worden om pp te berekenen.

Acties voor opstarten:

-   Dick stuurt rapport op, plus de gegevens. (done)
-   Willem maakt plan van aanpak, stuurt rond voor commentaar (bij deze)
-   Willem maakt voorstel voor poster op Scheldesymposium (overzicht primaire productiemetingen).
-   Willem vraag RWS en UA om gegevens PP metingen (gedaan, nog geen antwoord, Nicole is op vakantie.)
-   Willem nodigt rest uit voor Github repos (done)
