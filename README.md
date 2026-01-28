# Treball Final de Consultoria Éstadística 2025

Andrea Acuña Villagaray (16393232) i Maria Marín Méndez (1668394)  


En aquest repositori es recull el desenvolupament del **Treball Final de Consultoria Estadística 2025**, centrat en l'anàlisi exhaustiva de la Loteria de Nadal des de la seva vessant més tècnica. L'objectiu no és sols descriure el sorteig, sinó avaluar amb rigor estadístic si la variabilitat dels resultats històrics (des del 2000 fins a l'anys 2025) respon purament a l'atzar o si existeixen anomalies mesurables en l'homogeneïtat del sistes,

A través de metodologies de web scraping, tests d'independència .....

La Loteria de Nadal no és sols un sorteig de boles; és l'unic moment de l'any en què un país sencer es posa d'acord per ignorar les lleis de l'estadística. Des d'un punt de vista matemàtic, és un "impost a l'esperança", però des del punt de vista de lesa dades, és un ecosistema fascinant.

**L'arquitectura del "GORDO"**

La loteira no ven números a l'atzar, sinó que segueix una jerarquia rígida que determina quants diners es mouen i quina probabilitat real tens de guanyar.

El sistema treballa amb un ventall de 100.000 números (des del 00000 fins al 99999). Això ens deixa una probabilitat de guanyar el primer premi amb un sol dècim de un 0.001%.

Per al 2025, SELAE (Societat Estatal Loteries i Apostes de l'Estat) va emetre 197 sèries de cada número. Això significa que de cada número n'hi ha 197 billets idèntic repartits per tot l'estat. On cada sèroe es dovodeox en 10 dècims, el que ens dona un total de 1970 dècims per cada número.

- Preu del dècim: 20€
- Recaptació potencila, si es ven tot, el sorteig mou 3940 milions d'euros. D'aquests, el 705 es destina a premis.

El que veiem a la tele és folkore, però el que hi ha dins dels bombos és física pura. com ens explica el professor Badiella, el sistema està dissenyat per se homogeni.

Hi ha 100.000 boles al bombo gran, totes fetes de gusta deboic, amb un diàmetre de 3cm i un pes unificat. On per evitar que el pes de la pintura alteri el camí de la bola cap a la trompeta, els números estan impresos amb l'àser. Així s'elimina la teoria que els números amb més "tinta", com el 88.888, pesen més que el 11.111.

Per a realtizar el sorteig es fan servis dos bombos que giren simultàniament: un per als números i un latre amb les 1807 boles de premis. fins que el bombo petit no queda buit, el sorteig no es dona per acabat.

**El repartiment del "Pastís"**

Tot i que el focus està en el Gordo, la realitat és que el sorteig és una pluja fina de premis petits que serveixen per "alimentar l'esperança" de cara a l'anys següent.

| Premi                   | Import per dècim | Boles premiades     | Probalitat  |
|-------------------------|------------------|---------------------|-------------|
| 1r  premi ("el Gordo")  | 400.000€         | 1                   | 0,001 %     |
| 2n  premi               | 125.000€         | 1                   | 0,001 %     |
| 3r  premi               | 50.000€          | 1                   | 0,001 %     |
| 4rt premi               | 20.000€          | 2                   | 0,002 %     |
| 5é premi                | 6.000€           | 8                   | 0,008 %     |
| La Pedrea               | 100€             | 1.794               | 1,794 %     |

| Premi                   | Import per dècim | Nº Premiat          | Probalitat  |
|-------------------------|------------------|---------------------|-------------|
| Aproximació 1r premi    | 200€             | 2                   | 0,002 %     |
| Aproximació 2r premi    | 125€             | 2                   | 0,002 %     |
| Aproximació 3r premi    | 96€              | 2                   | 0,002 %     |
| Terminació              | 100€             | 1000 * 3            | 3,000 %     |
| Centena                 | 100€             | 100 * 5             | 0,500 %     |
| Reintegrament           | 20€              | 1 de cada 10 xifres | 10,00 %     |



Aleshores amb aquesta informació la probabilitat de que et toqui el Gordo és la mateixa que la de qualsevol número, 0,001%. Gairebé el 99% dels números que reben premi directe del bombo són pedres de 100€ (1.794 de 1.807). Per altra banda, tenim un 10% de possibilitats de recuperar 20€ si l'última xifra del teu número coincideix amb la del Gordo.

Hem de recordar que per als tres primers premis, el premi realment és menor, ja que Hisenda entra al joc aplicant un 20% d'impost a qualsevol premi que superi els 40.000€. Això vol dir que, el guanyador del primer premi realment es de 328.000€, ja que els premiers 40.000€ estan exempts i es paga 20% dels 360.000€ restant. El segon premi sseria de 108.000€ i del trecer 48.000€.








Per on podem començar l'anàlisi?
1. Històric de terminacions: quins números han sortir més.
2. Geografia de la sort: Analitzar si és veritat que el premi toca més a Madrid o Barcelona, o si és simplement perquè s'hi venene més dècims.
3. Simulació de Montecarlo (proposta gemini): crear un script en R que simuli 10.000 sortejos per veure queantes vegades guanyaries el Gordo si juguessis el mateix número durant 100 anys.

