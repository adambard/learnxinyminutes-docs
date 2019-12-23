---
category: Algorithms & Data Structures
name: Dynamic Programming
contributors:
    - ["Akashdeep Goel", "http://github.com/akashdeepgoel"]
translators:
  - ["Jasper Haasdijk", "https://github.com/jhaasdijk"]
lang: nl-nl
---

# Dynamisch Programmeren

## Introductie

Dynamisch programmeren is een krachtige techniek die, zoals we zullen zien, gebruikt kan worden om een bepaalde klasse van problemen op te lossen. Het idee is eenvoudig. Als je een oplossing hebt voor een probleem met een bepaalde input, sla dit resultaat dan op. Hiermee kan je voorkomen dat je in de toekomst nog een keer hetzelfde probleem moet gaan oplossen omdat je het resultaat vergeten bent.

Onthoud altijd!
"Zij die het verleden niet kunnen herinneren, zijn gedoemd het te herhalen."

## Verschillende aanpakken

1. *Top-Down* : Oftewel, van boven naar beneden. Begin je oplossing met het afbreken van het probleem in kleine stukken. Kom je een stukje tegen dat je eerder al hebt opgelost, kijk dan enkel naar het opgeslagen antwoord. Kom je een stukje tegen dat je nog niet eerder hebt opgelost, los het op en sla het antwoord op. Deze manier is voor veel mensen de makkelijke manier om erover na te denken, erg intuitief. Deze methode wordt ook wel Memoization genoemd.

2. *Bottom-Up* : Oftewel, van beneden naar boven. Analyseer het probleem en bekijk de volgorde waarin de sub-problemen opgelost kunnen worden. Begin met het oplossen van de triviale gevallen en maak zodoende de weg naar het gegeven probleem. In dit process is het gegarandeerd dat de sub-problemen eerder worden opgelost dan het gegeven probleem. Deze methode wordt ook wel Dynamisch Programmeren genoemd.

## Voorbeeld van Dynamisch Programmeren

Het langst stijgende sequentie probleem is het probleem waarbij je binnen een bepaalde reeks op zoek bent naar het langste aaneengesloten stijgende stuk. Gegeven een reeks `S = {a1 , a2 , a3, a4, ............., an-1, an }` zijn we op zoek naar het langst aaneengesloten stuk zodanig dat voor alle `j` en `i`, `j<i` in de reeks `aj<ai`.

Ten eerste moeten we de waarde van de langste subreeksen(LSi) op elke index i vinden waar het laatste element van de reeks ai is. Daarna zal LSi het langste subreeks in de gegeven reeks zijn. Om te beginnen heeft LSi de waarde 1 omdat ai een element van de reeks(laatste element) is. Daarna zal voor alle `j` zodanig dat `j<i` en `aj<ai` de grootste LSj gevonden en toegevoegd worden aan LSi. Het algoritme duurt *O(n2)* tijd.

Pseudo-code voor het vinden van de lengte van de langst stijgende subreeks:
De complexiteit van het algoritme kan worden vermindert door het gebruik van een betere data structuur dan een simpele lijst. Het opslaan van een voorgangers lijst en een variabele als `langste_reeks_dusver` en de index daarvan, kan ook een hoop tijd schelen.

Een soortgelijk concept kan worden toegepast in het vinden van het langste pad in een gerichte acyclische graaf.

```python
for i=0 to n-1
    LS[i]=1
    for j=0 to i-1
        if (a[i] >  a[j] and LS[i]<LS[j])
            LS[i] = LS[j]+1
for i=0 to n-1
    if (langste < LS[i])
```

### Enkele beroemde DP problemen

- [Floyd Warshall Algorithm - Tutorial and C Program source code](http://www.thelearningpoint.net/computer-science/algorithms-all-to-all-shortest-paths-in-graphs---floyd-warshall-algorithm-with-c-program-source-code)
- [Integer Knapsack Problem - Tutorial and C Program source code](http://www.thelearningpoint.net/computer-science/algorithms-dynamic-programming---the-integer-knapsack-problem)
- [Longest Common Subsequence - Tutorial and C Program source code](http://www.thelearningpoint.net/computer-science/algorithms-dynamic-programming---longest-common-subsequence) 

## Online Bronnen

* [codechef](https://www.codechef.com/wiki/tutorial-dynamic-programming)
