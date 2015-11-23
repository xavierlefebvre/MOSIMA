globals [
  U                        ;; number of unemployed people
  V                        ;; number of vacant jobs
  L                        ;; labour force ( U + number of employed people)
  uLevel                   ;; unemployment level ( L - U)
  ur                       ;; unemployment rate ( U / L )
  vr                       ;; vacancy rate ( V / L )
  p                        ;; participation rate ( L / size of the adult civilian population in working age )
  number-skills            ;; vector size of the skills attribute for companies and people (constant)
]

breed [companies company]
breed [people person]

turtles-own [              ;;          COMPANY           /     PERSON
  skills                   ;; skills for a job           /  person skills          (vector of number-skills booleans)
  location                 ;; job location               /  person location        (discrete value)
  salary                   ;; job retribution            /  expected salary        (minimum salary S) resp.)
  state                    ;; filled/vacant              /  employed/unemployed    (true/false)
  productivity             ;; min productivity accepted  /  person's productivity
]

people-own [
  resignation-value        ;; value that may make someone want to resign ( 1 is the highest 0 the lowest [ the lower the value the more the person will wanna resign] )
  employement-tick         ;; simulation
]


to generateBeveridgeCurve
  set-current-plot "Market's Tightness"
  set-current-plot-pen "beveridge-pen"
  foreach [100 200 300 400] [
    set number-people ?
    foreach [100 200 300 400] [
      set number-companies ?
      setup
      let vr-sample []
      let ur-sample []
      while[ (ticks < max-number-simulation) and ( (length vr-sample < 2) or (abs 1 - (mean butlast vr-sample / mean butfirst vr-sample)  > sample-variation and abs 1 - (mean butlast ur-sample / mean butfirst ur-sample)  > sample-variation) )][
        go
        set vr-sample fput vr vr-sample
        set ur-sample fput ur ur-sample
        if (length vr-sample > sample-size + 1)[
          set vr-sample butlast vr-sample
          set ur-sample butlast ur-sample
        ]
      ]
      plotxy mean vr-sample mean ur-sample
    ]
  ]
end

to go
  learning
  regular-firing
  unexpected-firing
  match
  update-global-variables
  update-display
  tick
end


;;
;; Setup Procedures
;;
to clear
  clear-all
  reset-ticks
end

to setup
  clear-globals
  clear-ticks
  clear-turtles
  clear-patches
  clear-drawing
  clear-output
  setup-constants
  setup-people
  setup-companies
  dispatch-turtles
  update-global-variables
  update-display
  reset-ticks
end

;; set up basic constants of the model
to setup-constants
  set number-skills 5
end

;; dispatch in circle turtles ordered by breed
to dispatch-turtles
  let interval-people (world-height - 2) / number-people
  let interval-companies (world-height - 2) / number-companies
  ask people[
    setxy (min-pxcor + 1) (1 + min-pycor + who * interval-people)
  ]
  ask companies [
    setxy (max-pxcor - 1) (1 + min-pycor + (who - number-people) * interval-companies)
  ]
end

;; create the companies, then initialize their variables
to setup-companies
  create-companies number-companies [
    set skills n-values number-skills [random 2]
    set location random number-locations
    set salary min-salary + (sum skills * (max-salary - min-salary) / number-skills ) + ifelse-value (random 2 = 1) [ random (max-salary - min-salary) / number-skills] [ -1 * random (max-salary - min-salary) / number-skills]
    if salary > max-salary [set salary max-salary]
    if salary < min-salary [set salary min-salary]
    set state false
    set productivity random-float 1
    set shape "house"
  ]
end

;; create people, then initialize their variables
to setup-people
  create-people number-people [
    set skills n-values number-skills [random 2]
    set location random number-locations
    set salary min-salary + (sum skills * (max-salary - min-salary) / number-skills ) + ifelse-value (random 2 = 1) [ random (max-salary - min-salary) / number-skills] [ -1 * random (max-salary - min-salary) / number-skills]
    if salary > max-salary [set salary max-salary]
    if salary < min-salary [set salary min-salary]
    set state false
    set productivity random-float 1
    set resignation-value 1
    set shape "person"
  ]
end

to update-global-variables
  set U count people with [ state = false ]
  set V count companies with [ state = false ]
  set L U + count people with [ state = true ]
  set uLevel L - U
  set ur U / L
  set vr V / L
  set p L / number-people
end

to update-display
  ask turtles [
    set color ifelse-value state [ green ] [ grey ]
  ]
end

;;
;; Person Procedures
;;

to random-resign [company]
  if (random 1 < 0.1) [ ;; We could had a variable instead of just using an  arbitrary value in this case but since this function is gonna be improve soon it didn't seemed useful.
    unlink-person-company self company
  ]
end

to resign [company]
  if (resignation-value < resignation-threshold) [
    unlink-person-company self company
      ]
  let fluctuation random-float max-resignation-fluctuation
  set resignation-value ifelse-value (random 2 = 1)[min list 1 (resignation-value + fluctuation) ] [max list 0 (resignation-value - fluctuation)]
end

to learning
  ask people with [ state = true] [
    if (employement-tick + learning-time = ticks ) [
      let company-skills [skills] of one-of link-neighbors
      let my-skills [skills] of self
      let turn 0
      repeat number-skills [
        if ( item turn my-skills != item turn company-skills) [
          if (item turn my-skills = 0) [ set my-skills replace-item turn my-skills 1]
        ]
        set turn turn + 1
      ]
    ]
    if (employement-tick + forgetting-time = ticks ) [
      let company-skills [skills] of one-of link-neighbors
      let my-skills [skills] of self
      let turn 0
      repeat number-skills [
        if ( item turn my-skills != item turn company-skills) [
          if (item turn my-skills = 1 ) [ set my-skills replace-item turn my-skills 0  ]
        ]
        set turn turn + 1
      ]
    ]
  ]
end

;;
;; Companies Procedures
;;
to regular-firing
  ask companies with [ state = true ] [
    ask link-neighbors [
      if ([productivity] of self / [productivity] of myself) < firing-quality-threshold [
        unlink-person-company self myself
      ]
      let fluctuation random-float max-productivity-fluctuation
      set productivity ifelse-value (random 2 = 1 )[min list 1 (productivity + fluctuation) ] [max list 0 (productivity - fluctuation)]
      if (link-neighbor? myself) [
        resign myself
      ]
    ]
  ]
end


to unexpected-firing
  ask companies with [ state = true ] [
    ask link-neighbors [
      if (random-float 1) < unexpected-firing-rate [
        unlink-person-company self myself
      ]
    ]
  ]
end

to unlink-person-company [turtle1 turtle2]
  ask turtle1 [
    set state false
  ]
  ask turtle2 [
    set state false
    ask link-with turtle1 [die]
  ]
end



;;
;; Matcher Procedures
;;
to match
  let number-pairs ceiling min list ( matching-random-pair-number * count people with [ state = false ] )( matching-random-pair-number * count companies with [ state = false ] )
  repeat number-pairs [
    let person one-of people with [ state = false ]
    let company one-of companies with [ state = false ]
    let similarity mean list
      (ifelse-value (random-float 1 < unexpected-worker-motivation)  [evaluate person company + max-motivation-fluctuation] [evaluate person company])
      (ifelse-value (random-float 1 < unexpected-company-motivation) [evaluate company person + max-motivation-fluctuation] [evaluate company person])
    if similarity >= matching-quality-threshold [
      link-person-company person company
      set-productivity-person person company
      ask person [
        set resignation-value 1
        set employement-tick ticks
      ]
    ]
  ]
end

to set-productivity-person [turtle1 turtle2]
  if (is-person? turtle1) and (is-company? turtle2) [
    let skills-required sum [skills] of turtle2
    let skills-matched length filter [ ? = true] (map [ ?1 = 1 and ?2 = 1] ([skills] of turtle1) ([skills] of turtle2))
    ask turtle1 [
      set productivity ifelse-value (skills-required > 0) [skills-matched / skills-required] [1]
    ]
  ]
  if (is-person? turtle2) and (is-company? turtle1) [
    let skills-required sum [skills] of turtle1
    let skills-matched length filter [ ? = true] (map [ ?1 = 1 and ?2 = 1] ([skills] of turtle2) ([skills] of turtle1))
    ask turtle2 [
      set productivity ifelse-value (skills-required > 0) [skills-matched / skills-required] [1]
    ]
  ]
end

to-report evaluate [turtle1 turtle2]
  report (mean (list (evaluate-skills turtle1 turtle2) (evaluate-location turtle1 turtle2) (evaluate-salary turtle1 turtle2) ) )
end

;; reports turtle1's skills evaluation
to-report evaluate-skills [turtle1 turtle2]
  report ((sum filter [? < 0] (map - [skills] of turtle2 [skills] of turtle1)) + number-skills) / number-skills
end

;; reports distance evaluation between turtle 1 and turtle2
to-report evaluate-location [turtle1 turtle2]
  report 1 - abs ( [location] of turtle1 - [location] of turtle2 ) / number-locations
end

;; reports turtle1's evaluation for the salary proposed by turtle2
to-report evaluate-salary [turtle1 turtle2]
  if (is-person? turtle1) and (is-company? turtle2) [
    report min list 1 ([salary] of turtle2 / [salary] of turtle1)
  ]
  report min list 1 ([salary] of turtle1 / [salary] of turtle2)
end

to link-person-company [turtle1 turtle2]
  ask turtle1 [
    set state true
  ]
  ask turtle2 [
    set state true
    create-link-with turtle1
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
267
30
694
781
16
28
12.64
1
10
1
1
1
0
0
0
1
-16
16
-28
28
0
0
1
ticks
30.0

BUTTON
714
29
769
62
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
14
97
245
130
number-people
number-people
0
400
400
1
1
NIL
HORIZONTAL

SLIDER
14
134
245
167
number-companies
number-companies
0
400
400
1
1
NIL
HORIZONTAL

BUTTON
772
29
827
62
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
13
615
244
648
number-locations
number-locations
1
100
10
1
1
NIL
HORIZONTAL

INPUTBOX
15
31
123
91
min-salary
100
1
0
Number

SLIDER
14
198
245
231
matching-quality-threshold
matching-quality-threshold
0
1
0.4
0.1
1
NIL
HORIZONTAL

SLIDER
13
302
243
335
firing-quality-threshold
firing-quality-threshold
0
1
0.5
0.1
1
NIL
HORIZONTAL

SLIDER
13
337
244
370
unexpected-firing-rate
unexpected-firing-rate
0
1
0.1
0.1
1
NIL
HORIZONTAL

SLIDER
13
372
244
405
max-productivity-fluctuation
max-productivity-fluctuation
0
1
0.4
0.1
1
NIL
HORIZONTAL

SLIDER
12
440
245
473
unexpected-company-motivation
unexpected-company-motivation
0
1
0
0.1
1
NIL
HORIZONTAL

SLIDER
12
476
244
509
unexpected-worker-motivation
unexpected-worker-motivation
0
1
0.1
0.1
1
NIL
HORIZONTAL

SLIDER
14
233
245
266
exceptional-matching
exceptional-matching
0
1
0.2
0.1
1
NIL
HORIZONTAL

SLIDER
12
578
244
611
matching-random-pair-number
matching-random-pair-number
0
1
0.6
0.1
1
NIL
HORIZONTAL

INPUTBOX
126
31
245
91
max-salary
200
1
0
Number

PLOT
714
102
989
303
Market's Tightness
vr - vacancy ratio
ur - unemployment rate
0.0
1.0
0.0
1.0
true
false
"" ""
PENS
"beveridge-pen" 1.0 2 -7500403 true "" ""

SLIDER
12
513
245
546
max-motivation-fluctuation
max-motivation-fluctuation
0
1
0.1
0.1
1
NIL
HORIZONTAL

BUTTON
830
29
987
62
Generate Beveridge curve
generateBeveridgeCurve
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
17
178
167
198
Matching parameters
16
0.0
1

TEXTBOX
15
281
165
301
Firing parameters
16
0.0
1

TEXTBOX
14
419
191
439
Motivation parameters
16
0.0
1

TEXTBOX
14
558
205
578
Environment parameters
16
0.0
1

TEXTBOX
19
10
169
30
Agents
16
0.0
1

BUTTON
714
65
987
98
Clear all
clear
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
14
682
144
742
max-number-simulation
500
1
0
Number

INPUTBOX
147
681
246
741
sample-size
100
1
0
Number

SLIDER
14
745
246
778
sample-variation
sample-variation
0
1
0.02
0.01
1
NIL
HORIZONTAL

TEXTBOX
15
662
214
688
Stop condition parameters
16
0.0
1

SLIDER
816
402
1021
435
resignation-threshold
resignation-threshold
0
1
1
0.05
1
NIL
HORIZONTAL

SLIDER
816
441
1045
474
max-resignation-fluctuation
max-resignation-fluctuation
0
1
0.2
0.1
1
NIL
HORIZONTAL

SLIDER
820
487
992
520
learning-time
learning-time
0
100
1
1
1
NIL
HORIZONTAL

SLIDER
807
563
979
596
forgetting-time
forgetting-time
0
100
50
1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

person business
false
0
Rectangle -1 true false 120 90 180 180
Polygon -13345367 true false 135 90 150 105 135 180 150 195 165 180 150 105 165 90
Polygon -7500403 true true 120 90 105 90 60 195 90 210 116 154 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 183 153 210 210 240 195 195 90 180 90 150 165
Circle -7500403 true true 110 5 80
Rectangle -7500403 true true 127 76 172 91
Line -16777216 false 172 90 161 94
Line -16777216 false 128 90 139 94
Polygon -13345367 true false 195 225 195 300 270 270 270 195
Rectangle -13791810 true false 180 225 195 300
Polygon -14835848 true false 180 226 195 226 270 196 255 196
Polygon -13345367 true false 209 202 209 216 244 202 243 188
Line -16777216 false 180 90 150 165
Line -16777216 false 120 90 150 165

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
