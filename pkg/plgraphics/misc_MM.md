Bemerkungen von Martin Mächler,  28.Juni 2023
===========

0.  Das ist wirklich *eindrücklich*  und schön.

[s. `/u/maechler/R/Pkgs/plgraphics.Rcheck-64b/00check.log` ]

1. Bei meinen eingeschalteten Extra "checks" bekomme ich

* checking R code for possible problems ... [24s/25s] NOTE
   check.numrange: local variable ‘lnx’ assigned but may not be used
   gendate: local variable ‘ltimeargs’ assigned but may not be used
   gendateaxis: local variable ‘lIym’ assigned but may not be used
   gendateaxis: local variable ‘lf.2char’ assigned but may not be used
   gendateaxis: local variable ‘ltickint’ assigned but may not be used
   i.genvattrcont: local variable ‘lvlim’ assigned but may not be used
   i.pchcens: local variable ‘ploptions’ assigned but may not be used
   leverage: local variable ‘ld’ assigned but may not be used
   pl.control : lf.xdt: local variable ‘lnc’ assigned but may not be used
   pl.control : lf.xdt: local variable ‘ltit’ assigned but may not be used
   pl.control: local variable ‘lcvform’ assigned but may not be used
   pl.control: local variable ‘lnsm’ assigned but may not be used
   plcond: local variable ‘lcynum’ assigned but may not be used
   plcond: local variable ‘lmain’ assigned but may not be used
   plcond: local variable ‘lmline’ assigned but may not be used
   plcond: local variable ‘lsub’ assigned but may not be used
   plcond: local variable ‘ltitcs’ assigned but may not be used
   plframe: local variable ‘grl’ assigned but may not be used
   plframe: local variable ‘llabline’ assigned but may not be used
   plframe: local variable ‘lmfg’ assigned but may not be used
   plframe: local variable ‘lxx’ assigned but may not be used
   plframe: local variable ‘lyy’ assigned but may not be used
   plmark: local variable ‘lmxnm’ assigned but may not be used
   plmatrix: local variable ‘lformy’ assigned but may not be used
   plmatrix: local variable ‘lgrp’ assigned but may not be used
   plmatrix: local variable ‘nvv’ assigned but may not be used
   plmboxes.default: local variable ‘f.ylim’ assigned but may not be used
   plmboxes.default: local variable ‘lirg’ assigned but may not be used
   plmboxes.default: local variable ‘ljlim’ assigned but may not be used
   ploptions: local variable ‘lop’ assigned but may not be used
   plpanelCond: local variable ‘lsmminobs’ assigned but may not be used
   plregr: local variable ‘condquant’ assigned but may not be used
   plregr: local variable ‘lcoef’ assigned but may not be used
   plregr: local variable ‘lform’ assigned but may not be used
   plregr: local variable ‘lnewplot’ assigned but may not be used
   plregr: local variable ‘lnnls’ assigned but may not be used
   plregr: local variable ‘lpch’ assigned but may not be used
   plregr: local variable ‘lrefline’ assigned but may not be used
   plregr: local variable ‘lsigma’ assigned but may not be used
   plregr: local variable ‘lsmgrp’ assigned but may not be used
   plregr: local variable ‘lsmooth’ assigned but may not be used
   plregr: local variable ‘lxn’ assigned but may not be used
   plregr.control: local variable ‘lInopsize’ assigned but may not be used
   plregr.control: local variable ‘lInosmweights’ assigned but may not be used
   plregr.control: local variable ‘lmodvdupl’ assigned but may not be used
   plregr.control: local variable ‘lsmweights’ assigned but may not be used
   plregr.control: local variable ‘lxvraw’ assigned but may not be used
   plresx: local variable ‘lIcomp’ assigned but may not be used
   plresx: local variable ‘lIjitter’ assigned but may not be used
   plresx: local variable ‘lIrpl’ assigned but may not be used
   plresx: local variable ‘lform’ assigned but may not be used
   plresx: local variable ‘ljitfac’ assigned but may not be used
   plresx: local variable ‘lrawv’ assigned but may not be used
   plresx: local variable ‘lweights’ assigned but may not be used
   plresx: local variable ‘mar’ assigned but may not be used
   plscale: local variable ‘ln’ assigned but may not be used
   pltitle: local variable ‘lIdoc’ assigned but may not be used
   pltitle: local variable ‘tcsmin’ assigned but may not be used
   plyx: local variable ‘lir’ assigned but may not be used
   plyx: local variable ‘lop’ assigned but may not be used
   plyx: local variable ‘lplabg’ assigned but may not be used
   prettyscale: local variable ‘lcall’ assigned but may not be used
   residuals.regrcoxph: local variable ‘lrs’ assigned but may not be used
   simresiduals.glm: local variable ‘lynm’ assigned but may not be used
   weekday: local variable ‘lnm’ assigned but may not be used

Dies ist alles nicht schlimm (und ist bei den Default-CRAN-checks auch
nicht eingeschaltet).

Es bedeutet aber z.B., dass du in der Funktion plresx()
interne Variable definierst, die fast sicher momentan überflüssig sind:
  lIcomp , lIjitter , lIrpl , lform , ljitfac , lrawv , lweights , mar

2. Beim Laden deines Pakets sehe ich dass deine Funktion last()  eine
   Funktion gleichen Namens im {sfsmisc}  maskiert....
   (diese ist einfacher als deine, und ist wirklich nur noch im {sfsmisc}
    drin, weil es schon seit Jahren so auf CRAN ist)

   Wenn ich im plgraphics suche, sehe ich, dass du diese recht häufig verwendest.
   Andererseits gibt es im R  head() und tail()
   und die beiden Funktionen wurden vor ~ 2 Jahren auch stark
   verallgemeinert, und für verschiedenste Objekte definiert, gut
   dokumentiert, weit verwendet, etc.

   Ich würde es didaktisch deshalb einiges sinnvoller finden, wenn du
   a) in den Beispielen (inkl Vignette und wohl auch tests/) ebenfalls
	   tail() / head() verwendest
   b) dein last() *nicht* mehr exportierst {aber beibehälst, damit du nicht
      überall deinen Code ändern musst}, und dir dadurch auch die Hilfeseite
      "sparen" kannst.
   c) so "nahdisnah" auch in deinem Code beginnst, tail() / head() zu verwenden.

3. example(plregr)  ist eindrücklich -- aber braucht mehr als 5 Sek was
   eine NOTE ergibt, so dass der ganze Status bei NOTE bleibt.
   Ich habe das jetzt verhindert, indem einzelne der Beispiele nur
   aufgerufen werden , "falls interaktiv".

4. Dort und noch bei Tobit:  Pakete wie  MASS oder survival sind zwar
   "Recommended", und daher eigentlich in jeder R Installation vorhanden.
   *Aber* trotzdem kann R ohne diese installiert werden, und wird es    z.T. auch.
   Deswegen "muss man" auch bei diesen Paketen checken, ob sie da sind,
   bei Beispiel und Tests.
   -->  verwende   if(requireNamespace("MASS"))  etc
   -----> habe ich jetzt schon gemacht.

5. Weil du bei  plregr.Rd  logischerweise 'regr' erwähnst, habe ich auch
   mal noch {relevance} geladen, und gesehen, dass es etliche
   "Konflikte" gibt:

```{r}
   > require(plgraphics)
   Loading required package: plgraphics

   Attaching package: ‘plgraphics’

   The following object is masked from ‘package:sfsmisc’:

	   last

   > require(relevance)
   Loading required package: relevance

   Attaching package: ‘relevance’

   The following objects are masked from ‘package:plgraphics’:

	   asinp, DB, dropdata, dropNA, last, logst, replaceNA, shortenstring, showd, sumNA

   The following object is masked from ‘package:sfsmisc’:

	   last

   > 
```

   Du hast da also 11 (wenn ich richtig geschaut) habe  Funktionen, die 
   sowohl im {relevance} als auch im {plgraphics}  definiert sind.
   Sicher hast das z.T. sehr bewusst so gemacht.
   
   Ich würde mir trotzdem gut überlegen, ob es nicht Sinn machen könnte,
   das nur in einem der beiden Pakete zu haben (und im andern von "dort" zu
   importieren, und wenn gewollt für den Benützer wieder zu exportieren.
   ... vielleicht am ehesten mündlich besprechen.


6. Es ist aus meiner Sicht unschön, dass du ein "binäres" File im R-forge
   committest, nämlich  `inst/doc/pl-description.pdf` .
   Das wird ja immer automatisch gemacht, und sollte eigentlich *nicht* in
   den Sourcen sein.
   Dasselbe gilt ("noch mehr") für `vignettes/pl-description.log` und auch
   `vignettes/pl-description.tex`  ....
   das war wohl sowieso nur ein Versehen, oder?

7a. Im PDF der Hilfe-Seiten,  `plgraphics-manual.pdf`, findet man etliche
   '$<math>$',  was natürlich "sub optimal" ist
   (z.B.  S.21, bei `gensmooth`, Argument `resid` : $y_i/\hat y_i$

7b. Ebenda finde ich 4 x  `!!!`  mit "to be described" oder ähnlich.
    Auch nicht dringend, aber "TODO" nicht zu vergessen.
	
7c. Titel 'ploptions' sagt 
>       Set and Get User "Session" Options that influence "regr0"s behavior
    da müsste "regr0" sicher ersetzt werden.	 
	


8. ?ploptions  sagt, dass das `.ploptions` im global environment sei.
   Das wäre *gravierend* (und nicht erlaubt für CRAN Pakete, und sehr
   _"frowned on"_ aber das weisst du ja).
   
   Zum Glück ist hier nur die Dokumentation veraltet, und `.ploptions` ist
   in der Tat dort wo es hingehört (im `plgraphics` namespace).
   
8b. Wenn ich  ` example(ploptions) ` laufen lasse, 
    werde ich "recht verwirrt":
	
	Der 1.Plot mit einem "falschen Ort für Text" .. und dann der 2.Plot, wo
    man gar keinen Text sieht -- weil die  par("mar") so setzt, dass es gar
    keine Platz für ein mtext() hat ...
	
9. "Aehnlich": Wenn ich `example(plregr)`  laufen lasse, kriege ich am Schluss	

```
 Warning messages:
 1: In par(loldpar) : argument 4 does not name a graphical parameter
 2: In par(loldpar) : argument 4 does not name a graphical parameter
 3: In par(loldpar) : argument 4 does not name a graphical parameter
 4: In par(loldpar) : argument 4 does not name a graphical parameter
 5: In par(loldpar) : argument 4 does not name a graphical parameter
 6: In par(loldpar) : argument 4 does not name a graphical parameter
 7: In par(loldpar) : argument 4 does not name a graphical parameter
```
   
  Das scheint für mich auf einen kleinen Bug im Code hinzuweisen ..


10. `pl-description` Vignette
-------------------

- 1.Satz:
>   The plotting functionality is the historical origin of the R package.
 
   "the R package" finde ich sehr unüblich -- wohl vor allem deshalb, weil 
   "R package" heute eine ganz andere Bedeutung hat:
    `plgraphics` ist *ein*  "R Package".
	
	Ich würde vielleicht schreiben   ".. origin of R, based on S"
	
- "technisch": Du verweist auf spätere Sections (S.3: "Section 2.2"),
     und da will ich im PDF drauf klicken können, d.h., einen
     richtigen Hyperlink haben, wie es ja "trivial" ist, im LaTeX zu
     bekommen.
	 
11. " 'pl' concept  "

 Du sprichst im Titel vom   " 'pl' concept "  
 dann von  " more computer science oriented ggplot concepts "
 (was eher schräg scheint; heutige Leser kennen ggplot2 bis zu einem
 gewissen Grade, und niemand denkt dabei an C.S)

 Dann kommt "concept" nur auf S.39 vor:  
	 "The concept of the ploptions list is ..."
 und wenn ich das etwas anschaue, verstehe ich langsam, dass in der Tat das
 Kapitel '5.  Specification of graphical elements '
 wohl die Beschreibung des früher erwähnten  "'pl' concept"  ist.
 
12. Farben / colors
-----
Hier erwähnst du (glaube ich) R's eigenes Koncept  von `palette()` und
zugehörigen Funktionen nicht direkt.

Hier gibt's eben schon viel (angewandte) "Forschung" und auch R Pakete etc etc.

Insbesondere war eine der diesbezüglich allerwichtigsten Innovationen mit R 4.0.0
die Einführung einer neuen Default-Palette, welche selber auf Team work und
Forschung und Feedback mit Experten (und auch dem was, ggplot2 macht)
beruht, und insbesondere Farben-Blindheit(en) etc berücksichtigt.

--> Z.B. der "R Blog" [https://blog.r-project.org/]
	zum Thema -- wo ich auch Co-Autor bin, zwar nicht "Driver", aber doch:
	
	https://blog.r-project.org/2019/11/21/a-new-palette-for-r/

Da wäre es vielleicht sinnvoll, deine Farb-Spezifikation im Lichte dieser recht
wichtigen Verbesserung im R noch zu revidieren, oder zumindest darauf Bezug
zu nehmen, oder?

Eine "palette active in the global environment"  (Sec 8.1, 1.Satz) 
gibt es jedenfalls nicht (die aktuelle Palette ist *nicht* im Global.env),
und im Lichte von dem, was wir im 2019 im obigen Blog geschrieben haben,
kam es eben dazu, dass die Wahl von Farben via *Namen* heute nicht mehr so
relevant scheint, wie es für uns früher immer war.

Der Default ist ja jetzt schon seit gut 3 Jahren

 > palette()
 [1] "black"   "#DF536B" "#61D04F" "#2297E6" "#28E2E5" "#CD0BBC" "#F5C710" "gray62" 

Du veränderst das ja ... ohne es zu erwähnen.
und offenbar kann der Benützer nicht auf "normale" Art eine andere
("bessere" .. siehe Blog, etc) Palette wählen, sondern müssen dazu den
"plgraphics -> ploptions" Weg gehen.
	
Man kann sagen, dass sei "ok":
- bei {lattice} gibt es "themes", die man setzen oder ändern kann;
- bei {ggplot2} ebenfalls "themes" (aber anders und "mehr ausgebaut" bzw
   mehr Funktionen); insbesondere auch via scale_**() Funktionsobjekte.

Ich habe etwas gelesen und gepröbelt und habe gefunden, dass

  ploptions(colors = palette.colors(palette="R4"))

offenbar funktioniert.  
Ich denke, das sollte in der Vignette und/oder auch bei ?ploptions erwähnt
werden.

Dabei gibt es aber noch einen Bug (meiner M. nach .. ja, "debatable"): 
Wenn ich -- wie ich's zuerst gemacht hatte -- schreibe

    ploptions(color = palette.colors(palette="R4"))

"scheint" alles gut {R sagt nichts} --- aber R macht gar nichts; weil ich
das "s" bei 'colors' vergessen habe.
Das ist nicht gut, es sollte eine Warnung geben *oder* erlauben, 
'colors' abzukürzen.

--> Sollte noch geflickt werden meiner Meinung nach *vor* dem Release.
