
<!-- README.md is generated from README.Rmd. Please edit that file -->

# README: Benckmark Scheme to Compare AI screening Performance with Humans

This repository contains all data and code behind the title and abstract
screening benchmark scheme developed in Vembye, Christensen, Mølgaard, &
Schytt ([2024](#ref-Vembye2024_gpt)). Below, we provide guidance on how
to find the data, code, and analyses behind this scheme. If you
experience any issue with reproducing our analysis, please contact
Mikkel Vembye at <mihv@vive.dk>.

# Data Folders

In the head folder you find a range of folders containing all screening
material for each of the 17 included Campbell Systematic Reviews. Each
folder contains the raw html screening data behind the review, the RIS
files for the included and excluded studies, and an R script in which
the individual screener performance data is created.

The specific folders are:  

- **Adult Child Ratio:** contains all material for Dalgaard, Bondebjerg,
  Klokker et al. ([2022](#ref-Dalgaard2022))
- **Asylum:** contains all material for Filges, Montgomery et al.
  ([2015](#ref-Filges2015b))
- **Class Size:** contains all material for Filges, Sonne-Schmidt et
  al. et al. ([2018](#ref-Filges2018))
- **Class Size Special Edu:** contains all material for Bondebjerg et
  al. ([2023](#ref-Bondebjerg2023))
- **Deployment:** contains all material for Bøg et al.
  ([2018](#ref-Bøg2018))
- **Development Training:** contains all material for Filges, Torgerson
  et al. ([2019](#ref-Filges2019))
- **Foster Adoptive:** contains all material for Dalgaard, Filges et al.
  ([2022](#ref-Dalgaard2022c))
- **FRIENDS** contains all material for Filges, Smedslund et al.
  ([2023](#ref-Filges2023))
- **Group-Based:** contains all material for Dalgaard, Jensen et al.
  ([2022](#ref-Dalgaard2022a))
- **Inclusion:** contains all material for Dalgaard, Bondebjerg,
  Viinholt et al. ([2022](#ref-Dalgaard2022b))
- **Outreach:** contains all material for Filges, Dalgaard et al.
  ([2022](#ref-Filges2022))
- **School Reviews:** contains all material for Dietrichson et al.
  ([2020](#ref-Dietrichson2020), [2021](#ref-Dietrichson2021))
- **Service Learning:** contains all material for Filges, Dietrichson et
  al. ([2022](#ref-Filges2022a))
- **Sport:** contains all material for Filges, Verner et al.
  ([2023](#ref-Filges2023a))
- **TF:** contains all material for Thomsen et al.
  ([2022](#ref-Thomsen2022))
- **Voluntary:** contains all material for Filges, Siren et al.
  ([2020](#ref-Filges2020))

Within each of the R scripts stored in the above folders, the single
screener data generated was saved to the **single screener data**
folder. The data sets used for the main analysis are the ones containing
the ’\_2screen.rds’ in its file text. All screening performances
analyses from the Campbell Systematic Reviews can be found in the
**Analyze single screen performance.R** file in the **single screener
data** folder.

## NIPH folder

The screening data used for analyzing the screening performance in the
NIPH (Norwegian Institute of Public Health) reviews is stored in the
NIPH folder.  

The NIPH folder contains the following subfolders:  

- **Data:** contains the single screeners performance data  
- **First episode psychosis:** contains all material for Jardim et al.
  ([2021](#ref-Jardim2021))
- **Joint custody:** contains all material for Johansen et al.
  ([2022](#ref-Johansen2022))
- **Psychological debriefing:** contains all material for Meneses
  Echavez et al. ([2022](#ref-MenesesEchavez2022))
- **Rotator cuff tears:** contains all material for Evensen et al.
  ([2023](#ref-Evensen2023))
- **Self help apps:** NOT USED. Represent an evidence-and-gap-map.
- **WHO QES Chronic Pain:** contains all material for Ames et al.
  ([2024](#ref-Ames2024))
- **Womens health:** Not used

Each folder contains the raw html screening data behind the review and
the RIS files for the included and excluded studies.  

All screening performances analyses from the NIPH reviews can be found
in the **NIPH data and analysis.R** file.

# Irrelavant Head Folders

- **Adult child ratio qualitative:** Not independently doubled screened
- **12-step:** Not independently doubled screened
- **co-teaching:** Not possible to obtained individual screener
  performance. Was extracted from Covidence

# References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0" line-spacing="2">

<div id="ref-Ames2024" class="csl-entry">

Ames, H., Hestevik, C. H., & Briggs, A. M. (2024).
<span class="nocase">Acceptability, values, and preferences of older
people for chronic low back pain management; a qualitative evidence
synthesis</span>. *BMC Geriatrics*, *24*(1), 1–22.
<https://doi.org/10.1186/s12877-023-04608-4>

</div>

<div id="ref-Bøg2018" class="csl-entry">

Bøg, M., Filges, T., & Jørgensen, A. M. K. (2018).
<span class="nocase">Deployment of personnel to military operations:
impact on mental health and social functioning</span>. *Campbell
Systematic Reviews*, *14*(1), 1–127.
<https://doi.org/10.4073/csr.2018.6>

</div>

<div id="ref-Bondebjerg2023" class="csl-entry">

Bondebjerg, A., Dalgaard, N. T., Filges, T., & Viinholt, B. C. A.
(2023). <span class="nocase">The effects of small class sizes on
students’ academic achievement, socioemotional development and
well‐being in special education: A systematic review</span>. *Campbell
Systematic Reviews*, *19*(3), e1345. <https://doi.org/10.1002/cl2.1345>

</div>

<div id="ref-Dalgaard2022" class="csl-entry">

Dalgaard, N. T., Bondebjerg, A., Klokker, R., Viinholt, B. C. A., &
Dietrichson, J. (2022). <span class="nocase">Adult/child ratio and group
size in early childhood education or care to promote the development of
children aged 0–5 years: A systematic review</span>. *Campbell
Systematic Reviews*, *18*(2), e1239. <https://doi.org/10.1002/cl2.1239>

</div>

<div id="ref-Dalgaard2022b" class="csl-entry">

Dalgaard, N. T., Bondebjerg, A., Viinholt, B. C. A., & Filges, T.
(2022). <span class="nocase">The effects of inclusion on academic
achievement, socioemotional development and wellbeing of children with
special educational needs</span>. *Campbell Systematic Reviews*,
*18*(4), e1291. <https://doi.org/10.1002/cl2.1291>

</div>

<div id="ref-Dalgaard2022c" class="csl-entry">

Dalgaard, N. T., Filges, T., Viinholt, B. C. A., & Pontoppidan, M.
(2022). <span class="nocase">Parenting interventions to support
parent/child attachment and psychosocial adjustment in foster and
adoptive parents and children: A systematic review</span>. *Campbell
Systematic Reviews*, *18*(1), e1209. <https://doi.org/10.1002/cl2.1209>

</div>

<div id="ref-Dalgaard2022a" class="csl-entry">

Dalgaard, N. T., Flensborg Jensen, M. C., Bengtsen, E., Krassel, K. F.,
& Vembye, M. H. (2022). <span class="nocase">PROTOCOL: Group‐based
community interventions to support the social reintegration of
marginalised adults with mental illness</span>. *Campbell Systematic
Reviews*, *18*(3), e1254. <https://doi.org/10.1002/cl2.1254>

</div>

<div id="ref-Dietrichson2020" class="csl-entry">

Dietrichson, J., Filges, T., Klokker, R. H., Viinholt, B. C. A., Bøg,
M., & Jensen, U. H. (2020). <span class="nocase">Targeted school-based
interventions for improving reading and mathematics for students with,
or at risk of, academic difficulties in Grades 7–12: A systematic
review</span>. *Campbell Systematic Reviews*, *16*(2), e1081.
<https://doi.org/10.1002/cl2.1081>

</div>

<div id="ref-Dietrichson2021" class="csl-entry">

Dietrichson, J., Filges, T., Seerup, J. K., Klokker, R. H., Viinholt, B.
C. A., Bøg, M., & Eiberg, M. (2021). <span class="nocase">Targeted
school-based interventions for improving reading and mathematics for
students with or at risk of academic difficulties in Grades K-6: A
systematic review</span>. *Campbell Systematic Reviews*, *17*(2), e1152.
<https://doi.org/10.1002/cl2.1152>

</div>

<div id="ref-Evensen2023" class="csl-entry">

Evensen, L. H., Kleven, L., Dahm, K. T., Hafstad, E. V., Holte, H. H.,
Robberstad, B., & Risstad, H. (2023). *<span class="nocase">Sutur av
degenerative rotatorcuff-rupturer: en fullstendig metodevurdering
\[Rotator cuff repair for degenerative rotator cuff tears: a health
technology assessment\].</span>* Retrieved from
<https://www.fhi.no/publ/2023/sutur-av-degenerative-rotatorcuff-rupturer/>

</div>

<div id="ref-Filges2022" class="csl-entry">

Filges, T., Dalgaard, N. T., & Viinholt, B. C. A. (2022).
<span class="nocase">Outreach programs to improve life circumstances and
prevent further adverse developmental trajectories of at-risk youth in
OECD countries: A systematic review</span>. *Campbell Systematic
Reviews*, *18*(4), e1282. <https://doi.org/10.1002/cl2.1282>

</div>

<div id="ref-Filges2022a" class="csl-entry">

Filges, T., Dietrichson, J., Viinholt, B. C. A., & Dalgaard, N. T.
(2022). <span class="nocase">Service learning for improving academic
success in students in grade K to 12: A systematic review</span>.
*Campbell Systematic Reviews*, *18*(1), e1210.
<https://doi.org/10.1002/cl2.1210>

</div>

<div id="ref-Filges2015b" class="csl-entry">

Filges, T., Montgomery, E., Kastrup, M., & Jørgensen, A.-M. K. (2015).
<span class="nocase">The impact of detention on the health of asylum
seekers: A systematic review</span>. *Campbell Systematic Reviews*,
*11*(1), 1–104. <https://doi.org/10.4073/csr.2015.13>

</div>

<div id="ref-Filges2020" class="csl-entry">

Filges, T., Siren, A., Fridberg, T., & Nielsen, B. C. V. (2020).
<span class="nocase">Voluntary work for the physical and mental health
of older volunteers: A systematic review</span>. *Campbell Systematic
Reviews*, *16*(4), e1124. <https://doi.org/10.1002/cl2.1124>

</div>

<div id="ref-Filges2023" class="csl-entry">

Filges, T., Smedslund, G., Eriksen, T., & Birkefoss, K. (2023).
<span class="nocase">PROTOCOL: The FRIENDS preventive programme for
reducing anxiety symptoms in children and adolescents: A systematic
review</span>. *Campbell Systematic Reviews*, *19*(4), e1374.
<https://doi.org/10.1002/cl2.1374>

</div>

<div id="ref-Filges2018" class="csl-entry">

Filges, T., Sonne‐Schmidt, C. S., & Nielsen, B. C. V. (2018).
<span class="nocase">Small class sizes for improving student achievement
in primary and secondary schools: A systematic review</span>. *Campbell
Systematic Reviews*, *14*(1), 1–107.
<https://doi.org/10.4073/csr.2018.10>

</div>

<div id="ref-Filges2019" class="csl-entry">

Filges, T., Torgerson, C., Gascoine, L., Dietrichson, J., Nielsen, C., &
Viinholt, B. A. (2019). <span class="nocase">Effectiveness of continuing
professional development training of welfare professionals on outcomes
for children and young people: A systematic review</span>. *Campbell
Systematic Reviews*, *15*(4), e1060. <https://doi.org/10.1002/cl2.1060>

</div>

<div id="ref-Filges2023a" class="csl-entry">

Filges, T., Verner, M., Ladekjær, E., & Bengtsen, E. (2023).
<span class="nocase">PROTOCOL: Participation in organised sport to
improve and prevent adverse developmental trajectories of at-risk youth:
A systematic review</span>. *Campbell Systematic Reviews*, *19*(2),
e1321. https://doi.org/<https://doi.org/10.1002/cl2.1321>

</div>

<div id="ref-Jardim2021" class="csl-entry">

Jardim, P. S. J., Borge, T. C., & Johansen, T. B. (2021).
*<span class="nocase">Effekten av antipsykotika ved
f<span class="nocase">ø</span>rstegangspsykose: en systematisk oversikt
\[The effect of antipsychotics on first episode psychosis\]</span>*.
Retrieved from
<https://fhi.no/publ/2021/effekten-av-antipsykotika-ved-forstegangspsykose/>

</div>

<div id="ref-Johansen2022" class="csl-entry">

Johansen, T. B., Nøkleby, H., Langøien, L. J., & Borge, T. C. (2022).
*<span class="nocase">Samv<span class="nocase">æ</span>rs-og
bostedsordninger etter samlivsbrudd: betydninger for barn og unge: en
systematisk oversikt \[Custody and living arrangements after parents
separate: implications for children and adolescents: a systematic
review\]</span>*. Retrieved from
<https://www.fhi.no/publ/2022/samvars--og-bostedsordninger-etter-samlivsbrudd-betydninger-for-barn-og-ung/>

</div>

<div id="ref-MenesesEchavez2022" class="csl-entry">

Meneses Echavez, J. F., Borge, T. C., Nygård, H. T., Gaustad, J.-V., &
Hval, G. (2022). *<span class="nocase">Psykologisk debriefing for
helsepersonell involvert i u<span class="nocase">ø</span>nskede
pasienthendelser: en systematisk oversikt \[Psychological debriefing for
healthcare professionals involved in adverse events: a systematic
review\]</span>*. Retrieved from
<https://www.fhi.no/publ/2022/psykologisk-debriefing-for-helsepersonell-involvert-i-uonskede-pasienthende/>

</div>

<div id="ref-Thomsen2022" class="csl-entry">

Thomsen, M. K., Seerup, J. K., Dietrichson, J., Bondebjerg, A., &
Viinholt, B. C. A. (2022). <span class="nocase">PROTOCOL: Testing
frequency and student achievement: A systematic review</span>. *Campbell
Systematic Reviews*, *18*(1), e1212. <https://doi.org/10.1002/cl2.1212>

</div>

<div id="ref-Vembye2024_gpt" class="csl-entry">

Vembye, M. H., Christensen, J., Mølgaard, A. B., & Schytt, F. L. W.
(2024). <span class="nocase">GPT API Models Can Function as Highly
Reliable Second Screeners of Titles and Abstracts in Systematic Reviews:
A Proof of Concept and Common Guidelines</span>. *Open Science
Framework*. <https://doi.org/10.31219/osf.io/yrhzm>

</div>

</div>
