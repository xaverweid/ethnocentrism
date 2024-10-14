;; TERMINOLOGY
;; Action: Cooperation or defection as the action in a single PD game. Values: C, D
;; Strategy: Way to decide use cooperation or defection in a PD game based on previous gaming history. Values: AC (Always Cooperate), TFTT (Tit-for-two-tats), TFT (Tit-for-tats), GT (Grim Trigger), AD (Always Defect),
globals [
  n-groups
]


turtles-own [
  tag
  tag-ind
  likeness
  ;; TODO-20241014: Add new strategies
  strategies
  communality
  type-agent
]


links-own [
  ;; The links should store the previous outcomes of previous PD game
  ;; It should be noted that if the link is undirected, end1 is always the older of the two turtles, that is, the turtle with the smaller who number.
  ;; TODO-20241014: Try to store more of the PD game outcome
  had-game? ; record if two turtles had PD game last round
  used-actions0 ; the strategies gamer 0 used over the last 1 ~ 100 rounds
  used-actions1 ; the strategies gamer 1 used over the last 1 ~ 100 rounds
]


to setup
  clear-all

  ;; Create turtles (agents) with 2 tags: majority (blue) and minority (green)
  set n-groups 2

  crt num-agents [
    ifelse (who < minority-proportion * num-agents)[
      set tag "minority"
      set tag-ind 0
      set color green
    ][
      set tag "majority"
      set tag-ind 1
      set color blue
    ]

    ;; Initialise the likeness list based on in-group and out-group likeness
    let likeness-list []
    repeat n-groups [
      set likeness-list lput init-likeness-outgroup likeness-list
    ]
    set likeness replace-item tag-ind likeness-list init-likeness-ingroup

    ;; Initialise strategies randomly for each group
    let strategy-list []
    repeat n-groups [
      set strategy-list lput one-of ["AC" "TFTT" "TFT" "GT" "AD"] strategy-list
    ]
    set strategies strategy-list

    ;; Setup common properties across groups
    set shape "person"
    set communality 0

    ;; Set type
    set type-agent "Undefined"
  ]
  ;; Arrange turtles in the world
  ;; TODO-20241010: Other arranging methods might look better?
  arrange-turtles

  ;; Link turtles using small-world network setting
  let max-who 1 + max [who] of turtles
  let sorted sort ([who] of turtles)
  foreach sorted[ [?1] ->
    ask turtle ?1 [
      let i 1
      repeat num-links [
        create-link-with turtle ((?1 + i) mod max-who)
        set i i + 1
      ]
    ]
  ]
  repeat round (rewire-prop * num-agents) [
    ask one-of turtles [
      ask one-of my-links [die]
      create-link-with one-of other turtles with [link-with myself = nobody]
    ]
  ]

  ;; Setup game record
  ask links [
    set had-game? false
    set used-actions0 []
    set used-actions1 []
  ]

  reset-ticks
end


to go
  ;; Prisoner's Dilemma game
  ask links [
    let gamers sort [who] of both-ends
    let gamer0 turtle (item 0 gamers)
    let gamer1 turtle (item 1 gamers)

    ;; Determine game initialisation based on reciprocal likeness
    ;; TODO-20241014: plot interaction number and percentage
    let likeness01 item ([tag-ind] of gamer1) ([likeness] of gamer0)
    let likeness10 item ([tag-ind] of gamer0) ([likeness] of gamer1)
    ifelse (random-float 1 < min list likeness01 likeness10)[
      ;; Run PD game
      ;; Update game record
      set had-game? true
      let strategy0 item [tag-ind] of gamer1 [strategies] of gamer0
      let strategy1 item [tag-ind] of gamer0 [strategies] of gamer1

      ;; Translate strategy to action and record the action
      let action0 get-action strategy0 used-actions1
      let action1 get-action strategy1 used-actions0
      ifelse length used-actions0 > 99 [set used-actions0 but-first (lput action0 used-actions0)][set used-actions0 lput action0 used-actions0]
      ifelse length used-actions1 > 99 [set used-actions1 but-first (lput action1 used-actions1)][set used-actions1 lput action1 used-actions1]

      ;; Update communality based on PD game outcome
      ask gamer0 [
        set communality PD-game action0 action1
      ]
      ask gamer1 [
        set communality PD-game action1 action0
      ]
    ][
      ;; No PD game this round
      set had-game? false
    ]
  ]

  ;; Revise strategy and structure
  ;; WARNING-20241010: There are two things I find unclear in the article:
  ;; (1) How does the W (structural rigidity/strategy update frequency affect the revise process?
  ;; (2) In strategy update, does the agent copy from linked agent with maximum neighbors / linked agent with maximum communality?
  ;; (3) In structral update, is link cutting independent from link forming?
  ask turtles [
    ;; Determine whether do structural or strategic revise
    ifelse (random 20 < W)[
      ;; Revise links
      ;; Check if any neighbor has more than 1 neighbor, choose revisee
      if (any? link-neighbors with [count link-neighbors > 1])[
        let who-revisee [who] of one-of link-neighbors with [count link-neighbors > 1]
        let communality-revisee [communality] of turtle who-revisee

        ;; Revise probability
        if (random-float 1 < 1 / (1 + exp (beta * (communality-revisee - communality))))[
          ;; Cut link
          if (cut-link (min list who who-revisee) (max list who who-revisee))[
            ask link (min list who who-revisee) (max list who who-revisee) [die]

            ;; Form new link
            let potential-new-neighbors turtles with [self != myself and not link-neighbor? myself and link-neighbor? turtle who-revisee]
            if any? potential-new-neighbors [
                let new-link-neighbor one-of potential-new-neighbors
                create-link-with new-link-neighbor
                ask link-with new-link-neighbor [
                  set had-game? false
                  set used-actions0 []
                  set used-actions1 []
                ]
             ]
          ]
        ]
      ]
    ][
      ;; Revise strategies
      if (any? my-links with [had-game? = true])[
        let links-revisee-potential my-links with [had-game? = true]

        ;; Find the neighbor that have the maximum number of neighbors
        ;; If there are multiple such neighbors, find the last one
        let max-neighbor-count 0
        let strategy-revisee nobody

        ask links-revisee-potential [
          let linked-turtle other-end
          let neighbor-count count [link-neighbors] of linked-turtle
          if (neighbor-count > max-neighbor-count) [
            set max-neighbor-count neighbor-count
            set strategy-revisee linked-turtle
          ]
        ]

        ;; Copy strategies
        set strategies [strategies] of strategy-revisee
      ]
    ]
  ]

  ;; Report the proportion of altruists, ethnocentrists, cosmopolitans, and egoists
  ask turtles[
    let coop-in 0
    let coop-out 0
    let def-in 0
    let def-out 0
    let tag-ind-this-end tag-ind
    let who-this-end who
    ask my-links [
      ifelse (tag-ind-this-end = [tag-ind] of other-end)[
        ifelse (who-this-end < [who] of other-end)[
          set coop-in coop-in + length (filter [s -> s = "C"] used-actions0)
          set def-in def-in + length (filter [s -> s = "D"] used-actions0)
        ][
          set coop-in coop-in + length (filter [s -> s = "C"] used-actions1)
          set def-in def-in + length (filter [s -> s = "D"] used-actions1)
        ]
      ][
        ifelse (who-this-end < [who] of other-end)[
          set coop-out coop-out + length (filter [s -> s = "C"] used-actions0)
          set def-out def-out + length (filter [s -> s = "D"] used-actions0)
        ][
          set coop-out coop-out + length (filter [s -> s = "C"] used-actions1)
          set def-out def-out + length (filter [s -> s = "D"] used-actions1)
        ]
      ]
    ]
    set type-agent determine-type coop-in coop-out def-in def-out
  ]


  tick

end


to-report get-action [strategy used-actions]
  let action "A"
  (ifelse
    strategy = "AC" [
      set action "C"
    ]
    strategy = "TFTT"[
      set action "C"
      if length used-actions > 1 [
        if last used-actions = "D" and last but-last used-actions = "D" [
          set action "D"
        ]
      ]
    ]
    strategy = "TFT"[
      set action "C"
      if length used-actions > 0 [
        if last used-actions = "D" [
          set action "D"
        ]
      ]
    ]
    strategy = "GT" [
      set action "C"
      if member? "D" used-actions [
        set action "D"
      ]
    ]
    strategy = "AD" [
      set action "D"
    ])
  report action
end


to-report PD-game [strategy0 strategy1]
  let payoff 0
  (ifelse
    strategy0 = "C" and strategy1 = "C" [
      set payoff 1 ; reward payoff (R)
    ]
    strategy0 = "C" and strategy1 = "D" [
      set payoff 0 - gamma ; sucker payoff (S)
    ]
    strategy0 = "D" and strategy1 = "C" [
      set payoff 1 + gamma ; temptation payoff (T)
    ]
    strategy0 = "D" and strategy1 = "D" [
      set payoff 0 ; punishment payoff (P)
  ])
  report payoff
end


to-report cut-link [who0 who1]
  ;; cut link if being defected or did not have game in the last round
  ifelse ([had-game?] of link who0 who1)[
    let opponent-action last [used-actions0] of link who0 who1
    ifelse opponent-action = "C"[
      report false
    ][
      report true
    ]
  ][
    report true
  ]
end


to-report determine-type [coop-in coop-out def-in def-out]
  let type-to-determine "Undefined"
  ifelse coop-in + def-in > 0 and coop-out + def-out > 0 [
    (ifelse
      coop-in / (coop-in + def-in) >= 0.5 and coop-out / (coop-out + def-out) >= 0.5 [
        set type-to-determine "Altruist"
      ]
      coop-in / (coop-in + def-in) >= 0.5 and coop-out / (coop-out + def-out) < 0.5 [
        set type-to-determine "Ethnocentrist"
      ]
      coop-in / (coop-in + def-in) < 0.5 and coop-out / (coop-out + def-out) >= 0.5 [
        set type-to-determine "Cosmopolitan"
      ]
      coop-in / (coop-in + def-in) < 0.5 and coop-out / (coop-out + def-out) < 0.5 [
        set type-to-determine "Egoist"
      ])
  ][
    set type-to-determine "Undefined"
  ]
  report type-to-determine
end


;; Arranging turtles in a circle
;; This command procedure is copied from model 'Granovetter's threshold model of collective behaviour' (https://modelingcommons.org/browse/one_model/4784#model_tabs_browse_info)
to arrange-turtles ;similar to layout-circle (sort turtles) (max-pxcor - 1)
  let the-turtles sort [who] of turtles
  let angle 360 / num-agents
  let dist max-pxcor - 1
  let i 0
  foreach the-turtles [ [?1] ->
    ask turtle ?1 [
      setxy (dist * cos (angle * i)) (dist * sin (angle * i))
    ]
    set i i + 1
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
210
10
977
778
-1
-1
23.0
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
-16
16
0
0
1
ticks
30.0

SLIDER
13
10
202
43
num-agents
num-agents
0
500
100.0
1
1
NIL
HORIZONTAL

SLIDER
13
49
202
82
minority-proportion
minority-proportion
0
0.49
0.2
0.01
1
NIL
HORIZONTAL

SLIDER
12
106
202
139
init-likeness-ingroup
init-likeness-ingroup
0
1
0.3
0.1
1
NIL
HORIZONTAL

SLIDER
12
150
202
183
init-likeness-outgroup
init-likeness-outgroup
0
1
0.8
0.1
1
NIL
HORIZONTAL

BUTTON
13
322
103
355
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
13
227
201
260
num-links
num-links
0
5
2.0
1
1
NIL
HORIZONTAL

SLIDER
13
268
201
301
rewire-prop
rewire-prop
0
1
0.2
0.1
1
NIL
HORIZONTAL

SLIDER
985
10
1157
43
gamma
gamma
0
20
3.0
1
1
NIL
HORIZONTAL

SLIDER
985
48
1157
81
beta
beta
0
0.01
0.002
0.001
1
NIL
HORIZONTAL

SLIDER
985
87
1157
120
W
W
0
20
10.0
1
1
NIL
HORIZONTAL

BUTTON
120
322
200
355
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

TEXTBOX
15
200
165
218
Small World Network
11
0.0
1

PLOT
986
133
1475
397
Had-game Proportion in All Links
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"Both" 1.0 0 -7500403 true "" "plot (count links with [had-game? = true]) / count links"
"Inter-group" 1.0 0 -8990512 true "" "plot (count links with [[tag-ind] of end1 != [tag-ind] of end2 and had-game? = true]) / count links"
"Intra-group" 1.0 0 -1664597 true "" "plot (count links with [[tag-ind] of end1 = [tag-ind] of end2 and had-game? = true]) / count links"

PLOT
986
412
1475
765
Agent Types
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"altruist" 1.0 0 -13840069 true "" "plot (count turtles with [type-agent = \"Altruist\"])"
"ethonocentric" 1.0 0 -1184463 true "" "plot (count turtles with [type-agent = \"Ethonocentrist\"])"
"cosmopolitan" 1.0 0 -11221820 true "" "plot (count turtles with [type-agent = \"Cosmopolitan\"])"
"egoist" 1.0 0 -955883 true "" "plot (count turtles with [type-agent = \"Egoist\"])"

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
NetLogo 6.4.0
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
