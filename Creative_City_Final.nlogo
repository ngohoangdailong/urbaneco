
;; *************************************************** MODEL OF CREATIVE CLASS IN CITY  ***********************************************************


globals [
  mean-pop-count
  high-dense-level
  n-number
  percent-educated            ;; % of pop with college education; this value used for plot, calculated in brain-drain procedure
  percent-educated-cr         ;; % of creative with college education; value used for plot, calculated in brain-drain procedure
  indexofgini
  percent-poor
  percent-middle
  percent-rich
  cr-space%
  mean-rent-all
  mean-income-all
  median-income-all
  mean-income-cr
  ]

turtles-own [
  new               ;; allows for turtles created as result of pop-growth rate to get attributes
  hood-start        ;; list of neighborhoods turtle belongs to based on turtle creation
  afford-rent       ;; can turtle afford rent as percentage of income yes (1) or no (0)
  satisfied?        ;; satisifed when they are not looking to move, can afford rent, and have similar neighbors of same color
  content-w-neighbor;; content with similar ratio of neighbors desired yes (1) or no (0)
  tol-var           ;; tolerance range of variance, higher tolerance, more range, lower tolerance less range (tolerance / 2)
  tolerance         ;; percent of similar neighbors desired
  sim-ratio         ;; percent of similar neighbors nearby
  similar-nearby    ;; number of turtles with similar tolerance on neighboring patches (similar tolerance is +/- 5 points)
  similar-here      ;; number of turtles on same patch with similar tolerance
  similar           ;; total number of similar tolerance turtles here and on neibhors
  other-nearby      ;; number of turtles outside tolerance range on neighbor patches
  other-here        ;; number of turtles outside of tolerance range here
  other-count       ;; total number of turtles here and on neighbors outside tolerance range
  total-nearby      ;; total number of turtles here and on neighbors
  creative          ;; yes/no+
  creative-h        ;; highly creative yes/no
  creative-m        ;; medium creative yes/no
  creative-l        ;; low creative yes/no
  celebrity-status  ;; makes the most partnerships
  income            ;; gamma or bimodal distribution
  income-start      ;; income assigned at model start - can be used to see how much money made 
  current-rent      ;; rent turtles pays at current tick
  rent-%            ;; calculation of rent from patch / income (percent of income that goes to rent)
  rent-yr           ;; calculation of rent per year
  p-savings         ;; placeholder - yes/no to invest to move or start business
  educated          ;; university degree yes/no
 ;;  employment-status ;;student, employed, notworking
 ;;  entrepreneurial   ;;yes/no desire to start own business, tried to get funding, training
  HighCreative?     ;; If true, the person is considered to be high-creative
  partnered?        ;; If true, the person is paired with an investment or creative inspiration partner.
  partner-timeshare ;; How long the person prefers to partner with investor/creative inspiration.
  partner           ;; The person paired up with for business venture or creative inspiration.
  ]

 
patches-own [
  neighborhood     ;; neighborhood region displayed
  outside          ;; not assigned a neighborhood
  hood-list        ;; list of neighborhoods overlapping
  landuse          ;; landuse value assigned ranges from 0-7
  amenities        ;; placeholder
  pop-count        ;; count of turtles on patch - used to classify patches as high-density residential
  pop-dens         ;; placeholder if wanted to specify density
  occupancy-start  ;; how many turtles were here at start
  %full            ;; % full based on pop-count compared to occupancy-start
  rent-base        ;; rent as assigned at model setup
  rent-current     ;; rent price for this patch at current time tick
  creative-space   ;; true/false
  creative-value   ;; sum of value adde by turtles with creativity visiting patch
  creative-dens-p  ;; the start-pop of creative people per patch
  num-satisfied-cr ;; number of satisfied creative here
  pop-count-cr     ;; creative population counts use to assign creative value and creative space
  pop-count-crt
  pop-count-cr-h   
  pop-count-cr-m
  pop-count-cr-ht   
  pop-count-cr-mt 
  pop-count-cr-n   ;; new pop count used to decline in number of creatives visit
  pop-count-cr-diff;; diff of count of creative pop to current count of creative pop if negative then gained value, otherwise decrease
  pop-count-cr-minus;; factor to subtract for loss of creative value
  ]


;; ***** TO SET UP ************************************
to setup           ;; Procedures that run when setup button is pressed
  clear-all
  setup-landuse
  setup-rent         
  reset-ticks
  create-people
  setup-people
  create-neighborhoods
  setup-creative-space
  check-affordability
  setup-high-dense
  check-similar-tolerance
  update-variables
end


;; ***** DEFINE PATCH LANDUSE VALUES ************************************
;; average landuse for select city ..... 
to setup-landuse
    ask patches [set landuse 0];;  0 undeveloped white 24%    
    ask n-of (1681 * 0.60) patches [set landuse 1] ;; 1 is residential light orange 31% ** this is where turtles can live    
    ask n-of (1681 * 0.1) patches [set landuse 2] ;; 2 is commercial yellow 6%                 
    ask n-of (1681 * 0.1) patches [set landuse 3] ;; 3 is can't develop eg. airport etc gray 6% 
    ;; landuse 4  place holder for high dense  which is set up after creating initial people             
    ask n-of (1681 * 0.1) patches [set landuse 5] ;; 5 is water light blue 19%                 
    ask n-of (1681 * 0.1) patches [set landuse 6] ;; 6 green areas 9%                 
end


;; ***** SETUP INITIAL RENTS ****************************

;; rents assigned to patches as a monthly rent (e.g. rent per time tick)
to setup-rent  ;; rent is average monthly rent in units
   ask patches 
     [set rent-base int random-normal avg-rent 10000 ;; assigns initial rent as normal distrubution around user specifed avg rent with variance of 10000 (Std Dev) 
      set rent-current rent-base]
end


;; ********CREATE INITIAL PEOPLE *******************************************************************

to create-people ;;used at set up only
   create-turtles start-pop [set creative-l 1 set size 1 set new 1] ;; creates user-specified start-pop of turtles low creative
end

;; ******** ASSIGN ATTRIBUTES TO PEOPLE AND NEW POP GROWTH ********************
to setup-people
  ;; population growth rate
   if ticks >= 1 [if pop-growth-rate < 0 [ask n-of ((count turtles * (abs pop-growth-rate / 100)) / 12) turtles [die]]] ;; population decline
   if ticks >= 1  [create-turtles ((count turtles * (pop-growth-rate / 100)) / 12) [set creative 0 set creative-l 1 set size 1 set new 1] ] ;; create new turtles
  
  ;; assign attributes to new turtles
                     
  ;; assign education   
    ask n-of (count turtles with [new = 1 ] * (%educated / 100)) turtles with [new = 1]  [set educated 1] 
 
  ;; move turtles to residential patch and assign neighborhood
    ask turtles with [new = 1]
        [move-to one-of patches with [landuse = 1 or landuse = 4]
          set hood-start [neighborhood] of patch-here]
    

     if ticks >= 1 [if allow-development = false 
        [ask turtles with [new = 1 and hood-start = 0] 
          [move-to one-of patches with [(landuse = 1 or landuse = 4) and outside = 0] 
           set hood-start [neighborhood] of patch-here]]]
 
  
 ;; assign income as gamma curve
  if income-dist = "gamma"
    [let income-diff (top10% - percapita)
    let income-avg ((top10% + 9 * percapita) / 10) ;;weighted average
    let income-variance ((income-diff * income-diff) / 2)
    let alpha ((percapita * percapita) / income-variance)
    let lambda (1 / (income-variance / income-avg))
    ask turtles with [new = 1] [ set income-start int random-gamma (alpha * 2.5) (lambda * 2) set income income-start]
    ask turtles with [income-start < 1] [set income-start 1 set income income-start]]
    
 ;; assign income as bimodal curve
    if income-dist = "bi-modal"
    [ask turtles with [new = 1] [set income-start int random-normal percapita 20000 set income income-start]
     ask n-of (count turtles with [new = 1] * .1) turtles with [new = 1] [set income-start int random-normal top10% 20000 set income income-start]
     ask turtles with [income-start <= 1] [set income-start 1 set income income-start]]

   ask turtles with [educated = 1] [set income income * 1.0083] ;; turtles with education earn 1% more income each year
     
 set percent-poor (count turtles with [income <= (median-income-all * .75)] / (count turtles) * 100)
 set percent-middle (count turtles with [income > (median-income-all * .75) and income <= (median-income-all * 4)] / (count turtles) * 100)
 set percent-rich (count turtles with [income > (median-income-all * 4)] / (count turtles) * 100)
 
;; assign rents to turtles
    ask turtles with [new = 1]
         [set current-rent [rent-current] of patch-here
          set rent-yr ([rent-current] of patch-here)
          set rent-% (rent-yr / income) * 100 ;; percentange of income that goes to rent 
           ifelse rent-% >= rent%-of-income [set afford-rent 0] [set afford-rent 1] ] ;; if rent/yr for this patch less than income, poverty threshold then afford

 ;; assign creativity (assume creatives have a little more income, but high creatives have much more)
    ask n-of (count turtles with [new = 1] / 10)  turtles with [new = 1]   ;; 10% of pop is medium creative people
       [set creative 1 set creative-m 1 set creative-h 0 set creative-l 0 set income (income * 1.02)]  
    ask n-of (count turtles with [new = 1] * (%PopHighCreative / 100)) turtles with [new = 1];; user specifies percent high creative - % of high creative assigned below
         [set creative 1 set creative-h 1 set creative-l 0 set creative-m 0 set income (income * 1.03)]  
 

 ;; assign attributes that affect behavior
    ask turtles with [new = 1]
      [set partnered? false
       set partner nobody ]
      
 ;; assign tolerance
    ask turtles with [new = 1] ;; assigns initial tolerance to new turtles as normal dist around mean
      [set tolerance int random-normal tolerance-for-others 10]
    ;; adds noise to model, ask 10% to be random tolerance 
      ask n-of (count turtles with [new = 1] * .1) turtles with [new = 1] [set tolerance random 100] 
     ask turtles with [new = 1]
     [set tol-var (tolerance * .25) ] ;; set similar range as weights tolerance by taking variance and multiplying by use specified tolerance 



 ;; makes the turtles not new anymore
    ask turtles [set new 0]

;; used for monitor plots on interface
set mean-income-all (mean [income] of turtles)
set median-income-all (median [income] of turtles)
set mean-income-cr (mean [income] of turtles with [creative = 1])

end  


;; ***** CREATE NEIGHBORHOODS *******************
to create-neighborhoods ;; create neighborhoods as 7 random circles
  ask patches [set hood-list list (50) (50)] ;;created list with 50 as 2 last items in list
  repeat 7 
   [set n-number n-number + 1    ;; iteration so can add name to neighborhood and add name
    ask n-of 1 patches        
    [let p self
     let a max list 7 round (random-normal 7 (1 * 7))     
     ask patches with [distance p <= a]
      [set neighborhood n-number
       set hood-list fput neighborhood hood-list  ;; a patch can be part of overlapping neighborhoods
       ask turtles-here with [hood-start = 0] [set hood-start n-number set color (orange + hood-start)] ] ;;for some strange reason removing the set color from here ruins the display
     ]]
   ask patches with [neighborhood = 0] [set outside 1 set hood-list fput neighborhood hood-list]

   ask turtles with [hood-start = 0] 
     [move-to one-of patches with [(landuse = 1 or landuse = 4) and neighborhood != 0] 
        set hood-start [neighborhood] of patch-here]
end


;; ***** SET UP CREATIVE SPACE *******************
to setup-creative-space ;; create creative space to start based on density of creatives present, assigns creative value, bumps up rent on creative-space
 ask patches [ 

    set pop-count count turtles-here
    set pop-count-cr-h count turtles-here with [creative-h = 1] ;; count high creative on patch
    set pop-count-cr-m count turtles-here with [creative-m = 1] ;; count medium creative
    set pop-count-cr count turtles-here with [creative = 1]     ;; count of creative people same as (pop-count-cr-h + pop-count-cr-m)
    set creative-dens-p creative-dens                           ;; user defines start-pop of creatives per patch used to define creative space (default is 3)

       ;;creates initial creative spaces when enough creative tutrles and not in an outside area (e.g. not in a neighbhorhood)
       ;;assigns rent as double cost and assigns creative value of 10 for high creative, and 5 for medium creative   
    if pop-count-cr >= creative-dens-p                           
       [if allow-development = true [set creative-space 1 set rent-base (rent-base * 2) set creative-value ((pop-count-cr-h * 10) + (pop-count-cr-m * 5))]     
        if allow-development = false and outside = 0 [set creative-space 1 set rent-base (rent-base * 2) set creative-value ((pop-count-cr-h * 10) + (pop-count-cr-m * 5))] ]       
   ]
end


;; ***** SET UP HIGH DENSE RESIDENTIAL LANDUSE = 4 *******************
to setup-high-dense
      ;; assign occupancy value
       ask patches [set occupancy-start pop-count]

       set mean-pop-count (mean [pop-count] of patches with [outside = 0]) ;;set mean as pop-density of city
       set high-dense-level int (mean-pop-count * 4)
         ask patches with [pop-count > 4] [set landuse 4]  ;; set landuse as high dense residential (landuse = 4) density is 5x of average
end



;; *******GO PROCEDURES *******************************************************************
to go        ;; Procedures that run when go button is pressed
  setup-people ;;to create new people from pop-growth rate
  brain-drain  ;; educated leave
  update-cr-land-value
  check-partner  ;;  CONTROLS TURTLE INTERACTION AND SPREADS CREATIVITY - high creative turtles get larger after inspring others, comment out to check just schelling segregation rules
  check-affordability
  check-similar-tolerance
  move-unsatisfied
  update-variables
  tick
end


;; ********* BRAIN-DRAIN  ************** 

to brain-drain  ;; some educated or creative turtles disappear or appear based on user-specified percent
;; if losing smart people
 if ticks > 1 [
 if %brain-drain > 0 ;; convert smart or creative people to low creative and uneducated
  [ask n-of (count turtles with [educated = 1 or creative = 1] * (%brain-drain / 100) / 12) turtles with [educated = 1 or creative = 1]
       [set educated 0 set creative 0 set creative-h 0 set creative-m 0 set creative-l 1 set income income-start] ]   ;; brain-drain expressed as changing turtle from educated or creative to non-educated or non-creative

;; if gaining smart people 
  if %brain-drain < 0;; when no pop growth, but neg brain-drain converts existing population to creative based on percent of existing educated
    [if (count turtles with [educated = 1] * ((abs %brain-drain / 100) / 12) > count turtles with [educated = 0])
     [ask n-of (count turtles with [educated = 1] * ((abs %brain-drain / 100) / 12)) turtles with [educated = 0]
      [set educated 1 set new 1 set income (income * 1.5) ;;increases income
         if creative = 1          
           [ask n-of (count turtles with [new = 1] / 20) turtles with [new = 1] [set creative-m 1 set creative 1 set new 0];; ask 10% of new brainy people to be medium creative 
            ask n-of (count turtles with [new = 1] * ((count turtles with [creative-h = 1] / count turtles) / 100)) turtles with [new = 1] 
               [set creative-h 1 set creative 1 set new 0] ;;creates new high creatives based on current % of high creatives
            ]
       set new 0 ]
      ] 
     ]
 ]   
  set percent-educated count turtles with [educated = 1] / (count turtles) * 100  ;; used for plots
  set percent-educated-cr count turtles with [educated = 1 and creative = 1] / (count turtles) * 100  ;; used for plots

end


;; ************ SEGREGATION / TOLERANCE OF OTHER (BY COLOR OF TURTLE DISPLAYED) ***************
to check-similar-tolerance  ;;checks color of neighbors and then tolerance for similiarity
   
     
   ask turtles
    [    
     set similar-nearby count (turtles-on neighbors ) with [(tolerance >= ([tolerance] of myself) - tol-var ) or (tolerance < ([tolerance] of myself) + tol-var ) ]   ;; set similar nearby when other turtles are within -/+ 5 points of your own tolerance
     set similar-here count (turtles-on patch-here ) with [(tolerance >= ([tolerance] of myself) - tol-var ) or (tolerance < ([tolerance] of myself) + tol-var ) ]   ;; set similar nearby when other turtles are within -/+ 5 points of your own tolerance
     set similar (similar-nearby + similar-here)
   
     set other-nearby count (turtles-on neighbors) with [(tolerance < ([tolerance] of myself) - tol-var ) or (tolerance > ([tolerance] of myself) + tol-var ) ]
     set other-here count (turtles-on patch-here ) with [(tolerance < ([tolerance] of myself) - tol-var ) or (tolerance > ([tolerance] of myself) + tol-var ) ]   ;; set similar nearby when other turtles are within -/+ 5 points of your own tolerance
     set other-count (other-nearby + other-here)
     
     set total-nearby (similar + other-count)
     if total-nearby <= 0 [set total-nearby 1]
     set sim-ratio ((similar / total-nearby) * 100)
     ]

   ;; turtle is content when the ratio of turtles on its patch and neighbor patches of similar tolerance range matches or is better than its threshold for tolerance  
     ;; note: turtles will stop moving when they can afford rent (and if segregation is on then they also must be content with their neighbors)
   ask turtles ;; if the similar neighbors ratio is at min range of your tolerance level as range based on diff from turtle tol and user-specifed-avg (allows ability to make pop more/less tol at each tick)
     [ifelse (sim-ratio <= (tolerance * ((100 + tolerance-for-others) / 100)))  ;; minimum threshold to be content      
             [set content-w-neighbor 1 ] [set content-w-neighbor 0]
             ]
 end


;; ********* STOP WHEN SATISFIED TURTLES AND TO TELL UNSATISFIED TURTLES TO MOVE *************
to move-unsatisfied    ;; turtles move randomly around landscape trying to find a place to be satisfied
 ;; note: turtles will stop moving when they can afford rent (and if segregation is on then -- they also must be content with their neighbors)
        
 if segregation = true   ;; turtle is satisfied when can afford rent and it's content with neighbors or has high tolerance
         [ask turtles with [(afford-rent = 1 and content-w-neighbor = 1)]
           [set satisfied? 1]]
 
 if segregation = false      
         [ask turtles with [afford-rent = 1]  ;;if can afford rent and satisifed, stop      
            [set satisfied? 1]]  
           
  ask turtles with [afford-rent = 0] ;; if you can't afford rent you are not satisfied
     [set satisfied? 0]
     
  ask turtles with [satisfied? != 1] [move]  ;; if you are not satisfied, then move

end
   
to move
    if restrict-movement-to-neighborhood = true ;;if restrict movement is on, move to patch within neighborhood.
          [rt random-float 360                      ;;turn right
            ifelse member? hood-start hood-list     ;; check to see if neighborhood of turtle is in patch list of neighborhoods that overlap that patch
                 [fd random-float 1]                ;; move forward if patch is in neighbhorhood list
                 ;;move to nearest patch in neighborhood if current patch is not part of the original neighborhood list
                 [let numb hood-start move-to min-one-of patches with [member? numb hood-list] [distance myself]]]                      
    if restrict-movement-to-neighborhood = false
          [rt random-float 360  fd random-float 1 ]  ;;if no restriction is on, keep moving    
end  
 

;;ASSIGN creative VALUE FOR HIGH CREATIVE AREAS******************
;; ********** Turtle Interaction with patches *****************
;; add value - if a high-creative turtle lands on high creative patch it add value of 10 to patch, medium turtles add 5
;; change color - if creative value is 50 it changes to darker color, when creative-value >= 100 then color is black
;; spread creative patches - when a patch has creative value 100 or more, its neighbor4 patches change to landuse=7 and magenta
;; however won't change landuse and color of those that are landuse canton, transit, water

to update-cr-land-value
   ask patches 
       [set pop-count (count turtles-here) ;; update pop density each time for all patches          
       set pop-count-cr (count turtles-here with [creative = 1])        ;; count creative turtles on patch
       if pop-count-cr >= creative-dens and allow-development = false and outside = 1 [set creative-space 1]
         ;; if # of creative turtles matches threshold, make it a creative space
  
        set num-satisfied-cr count turtles-here with [creative = 1 and satisfied? = 1] ;; count satisfied creative turtles       
        set pop-count-cr-m count turtles-here with [creative-m = 1] ;; count creative turtles of medium on this patch at this time
        set pop-count-cr-h count turtles-here with [creative-h = 1] ;; count creative turtles of medium on this patch at this time
      
        ;; counter to subtract value for every 5 ticks no creative visit
        if pop-count-cr = 0  [set pop-count-cr-diff pop-count-cr-diff + 1] ;; if no creative turtles visit, add one to counter called diff
          if ticks > 1 and pop-count-cr-diff > 3 [set pop-count-cr-minus (pop-count-cr-minus + 1) set pop-count-cr-diff 0] ;;after 3 mos. no creative visit, add to minus counter, reset diff to zero
  
        if creative-space = 1 ;; for creative patches calculate value
        [    
          set pop-count-cr-ht (pop-count-cr-ht + (count turtles-here with [creative-h = 1]))  ;; count tally of high creative visits over time
          set pop-count-cr-mt (pop-count-cr-mt + (count turtles-here with [creative-m = 1]))  ;; count tally of medium creative visits over time
          set pop-count-crt (pop-count-cr-mt + pop-count-cr-ht)
        ] ;; count total creative turtles visits over time
          
 ;; need to work on this section
        ifelse num-satisfied-cr = pop-count-cr [set creative-value creative-value] ;; calculate creative value: each high creative 10, medium 5; subtract the count for minus
         [set creative-value ((pop-count-cr-ht * 10) + (pop-count-cr-mt * 5) - (pop-count-cr-minus * 100))]  
        
         set creative-value (creative-value * (pop-count-cr / creative-dens))
        
         if creative-value <= 1  [set creative-space 0 set creative-value 0]             
         if creative-value > 1 and creative-value < 50 [set creative-space 1]
         if creative-value >= 50 and creative-value < 100 [set rent-current (rent-base * 1.05)] 
         if creative-value >= 100 [set rent-current (rent-base * 1.1)] 
         if creative-value >= 300 [set rent-current (rent-base * 1.5)]
         if creative-value >= 500 
              [set creative-value 500 set rent-current (rent-base * 2)
               ask neighbors [ifelse allow-development = true 
                                 [if (creative-space != 1) [set creative-space 1]] 
                                 [ if (creative-space != 1 and outside != 1 and landuse != 4 and landuse != 5 and landuse != 3) [set creative-space 1 ]]]  ;;change neighbors4 to creative space.
                ]
         
          
      
     set cr-space% (count patches with [creative-space > 0] / 1681) * 100

]
        end

;; ******** INTERACTION BETWEEN TURTLES **********
;; PEOPLE INSPIRE AND INVEST IN OTHERS

to check-partner  ;; all turtles can partner if not partnered, look to couple, inspire, and uncouple
  ask turtles
    [if not partnered? and (random-float 10.0 < 2) [find-partner] 
    inspire
    uncouple]
end

;; probability of if two red meet in a creative patch they are most likely to create something
;; if red and blue interact probabliity is lower
;; if green and red match (if green is high risk taking (and high inome and ....) then  maybe coud be come blue

to find-partner ;; and to change and sprout
    let potential-partner one-of (turtles-at -1 0) with [not partnered?] ;; ask turtles at or near space if partnered then checks parameters for partnering
  if potential-partner != nobody
 ;; if turtle is creative-h 1 and 
   [ if random-float 10.0 < 2 ;;[intro-extro-tend] of potential-partner ;; consider removing????????  based on percentages ....to allow partner/change etc
      [ set partner potential-partner
        set partnered? true
        ask partner [ set partnered? true ]
        ask partner [ set partner myself ]
        move-to patch-here ;; move to center of patch
        move-to patch-here ;; partner moves to center of patch
      ]
   ]
end

;; 
to inspire  ;; to raise creativiy level  of individuals
  if creative-h = 1 and partnered?   ;;if 2 high creatives meet in area of creative-space they increase income
      [if [creative-h] of partner = 1 and [creative-value] of patch-here >= 500 [set income (income * 1.05) set celebrity-status (celebrity-status + 1)]  ;; those that partner a lot become "celebrity"
           if celebrity-status > 2 [set size 2] ];;those that are partnering most get bigger for visual     
  if creative-m = 1 and partnered?  ;; if medium creative partners with high creative, on area of amenities then it becomes a high creative and gains a little income
      [if [creative-m] of partner = 1 and [creative-value] of patch-here >= 500 [set creative 1 set creative-h 1 set creative-m 0 set income (income * 1.02)]]          
  if creative-m = 1 and partnered?  ;; if medium creative partners with high creative, on area of amenities then it becomes a high creative and gains a little income
      [if [creative-h] of partner = 1 and [creative-value] of patch-here >= 300 [set creative 1 set creative-h 1 set creative-m 0 set income (income * 1.02)]]          
  if creative-l = 1 and partnered?  ;; if low creative partners with high creative, on area of amenities then it gets bigger
       [if [creative-m] of partner = 1 and [creative-value] of patch-here > 100 [set creative 1 set creative-m 1 set creative-l 0 set income (income * 1.02)]]
  if creative-l = 1 and partnered?  ;; if low creative partners with high creative, on area of amenities then it gets bigger
       [if [creative-h] of partner = 1 and [creative-space] of patch-here = 1 [set creative 1 set creative-m 1 set creative-l 0 set income (income * 1.02)]]
 
end


;; placeholder probability of starting business
to start-business
end


to uncouple  ;; turtle procedure
  if partnered?
        [ set partnered? false
          ask partner [ set partner-timeshare 0 ]
          ask partner [ set partner nobody ]
          ask partner [ set partnered? false ]
          set partner nobody ] 
end


;; *********** CHECK affordability and occupancy and update rent *************
to check-affordability

  ;;rent increases 1% each year note that currently only turtles with education gain 1% income each year, turtles that partner also gain income
  ask patches [
     if ticks >= 1 [set rent-current (rent-current * 1.0083) set rent-base (rent-base * 1.0083)] 
     ]  

   ask turtles
    [if [landuse] of patch-here = 1 
       [if allow-development = false 
         [if [outside] of patch-here = 1 [set afford-rent 0]]]
       if allow-development = true 
         [set current-rent [rent-current] of patch-here
          set rent-yr ([rent-current] of patch-here * 1)
           ifelse rent-yr > income [set rent-% 100] [set rent-% (rent-yr / income) * 100]
           ifelse rent-% > 40 [set afford-rent 0] [set afford-rent 1] 
           ] ;; if rent/yr for this patch less than income, poverty threshold then afford
       ]
 ask turtles with [creative = 1]  ;; creatives don't have the same poverty threshold to account for their preference to be there (e.g. willing to pay a little higher)
  [if [creative-space] of patch-here = 1 
    [if (([rent-current] of patch-here * 1) < income) [set afford-rent 1]
      ]
    ]        
end


;; *********** UPDATE VARIABLES *************

to update-variables
  check-display
  plot-histograms
end

to check-display
;; to update-Display and turtles colors
ask patches
  [if _Display = "none"  [set pcolor gray + 3]  

  if _Display = "Landuse"
  [if landuse = 0 [set pcolor white] ;; undeveloped
   if landuse = 1 [set pcolor orange + 4] ;; residential
   if landuse = 2 [set pcolor yellow + 3] ;; commercial 
   if landuse = 3 [set pcolor gray + 2] ;; off-limits 
   if landuse = 4 [set pcolor orange + 2] ;; high dense residential
   if landuse = 5 [set pcolor 109] ;; water
   if landuse = 6 [set pcolor green + 3] ];; green space
  
  if _Display = "Neighborhoods"
  [set pcolor neighborhood + 1.5
    if neighborhood = 0 [set pcolor white]]  ;; gray scale

  if _Display = "Rents"
  [ set mean-rent-all (mean [rent-current] of patches)     
    if rent-current <= (mean-rent-all * .9) [set pcolor 75 + 4] ;; below average rent
    if rent-current >= (mean-rent-all * .9) [set pcolor 75 + 2] ;; average rent
    if rent-current >= (mean-rent-all * 1.5) [set pcolor 75 - 2] ] ;; really above avg and highest rent

  if _Display = "creative potential"
  [ if creative-space = 0 [set pcolor gray + 3]
    if creative-space = 1 [set pcolor 129]]
   ]

;; show creative spaces
  if show-creative-space = true
   [ask patches
   [ if creative-value > 0 and creative-value < 50 [set pcolor 126]  ;; set creative-space 1] ;;color patch based on value
     if creative-value >= 50 and creative-value < 100 [set pcolor 125] ;; set rent (rent * 1.05)]  ;; pink = pos
     if creative-value >= 100 [set pcolor 124] 
     if creative-value >= 300 [set pcolor 123]
      if creative-value >= 500 [set pcolor 121]
     ]]

;; to update turtles color
ask turtles
 [ ifelse _Turtle_color = "hide" [set hidden? true] [set hidden? false]  
   if _Turtle_color = "black" [set color black] 
   if _Turtle_color = "by Neighborhood"  [set color (orange + hood-start - 1)]
   if _Turtle_color = "afford rent" [ifelse afford-rent = 1 [set color red] [set color black]]
 
   if _Turtle_color = "Creative"
   [if creative-h = 1 [set color magenta]
   if creative-m = 1 [set color magenta + 2]
   if creative-l = 1 [set color green - 2]]
     
  if _Turtle_color = "Income" 
    [if income < (percapita * .9) [set color black + 2]
    if income >= (percapita * .9) and income <= (percapita * 1.1) [set color blue]
    if income > (percapita * 1.1) and (income <= 75000) [set color blue]
    if income > 75000 and income < (top10% * .8) [set color red]
    if income >= (top10% * .8) and income < (top10% * 1.2) [set color red]
    if income >= (top10% * 1.2)  [set color red]]
    
   if _Turtle_color = "Tolerance" ;; for trying to see whether the new tolerance thing works
    [if tolerance >= (tolerance-for-others + (tolerance-for-others * .25)) [set color blue]  ;; more tolerant
     if tolerance <= (tolerance-for-others + (tolerance-for-others * .25)) [set color green] ;; average tolerance
     if tolerance <= (tolerance-for-others - (tolerance-for-others * .25)) [set color red] ] ;; low tolerance 
   
   
   if _Turtle_color = "Content" ;; for trying to see whether the new tolerance thing works
   [ ifelse content-w-neighbor = 1 [set color green - 2]  [set color red + 2]]
  ]
end


to plot-histograms
 set-current-plot "Lorenz Curve"
 clear-plot
 set-current-plot-pen "Equal"  ;; draw a straight line from lower left to upper right
 plot 0
 plot 100
 set-current-plot-pen "Lorenz"
 set-plot-pen-interval 100 / count turtles
 plot 0

 let SortedWealths sort [Income] of turtles
 let TotalWealth sum SortedWealths
 let WealthSumSoFar 0
 let GiniIndex 0
 let GiniIndexReserve 0

 repeat count turtles [
 set WealthSumSoFar (WealthSumSoFar + item GiniIndex SortedWealths)
 plot (WealthSumSoFar / TotalWealth) * 100
 set GiniIndex (GiniIndex + 1)
 set GiniIndexReserve GiniIndexReserve + (GiniIndex / count turtles) - (WealthSumSoFar / TotalWealth)]
;; plot GiniIndex
;; set-current-plot "Gini Index"
 set IndexOfGini ((GiniIndexReserve / count turtles) / 0.5)
;; plot (GiniIndexReserve / count turtles) / 0.5

  set-current-plot "Income Distribution"
  set-current-plot-pen "Income-dist"
  if income-dist = "gamma" [set-histogram-num-bars 500]
  if income-dist = "bi-modal" [set-histogram-num-bars 100]
  histogram [income] of turtles
  set-current-plot-pen "Income-cr"
  if income-dist = "gamma" [set-histogram-num-bars 500]
  if income-dist = "bi-modal" [set-histogram-num-bars 100]
  histogram [income] of turtles with [creative = 1]
  
  set-current-plot "Rent Distribution"
  set-current-plot-pen "Creative"
  set-histogram-num-bars 100
  histogram [rent-current] of patches with [creative-space = 1]
  set-current-plot-pen "Overall"
  set-histogram-num-bars 1000
  histogram [rent-base] of patches 

  set-current-plot "tol-var"
  set-current-plot-pen "tol-var"
  set-histogram-num-bars 100
  histogram [tol-var] of turtles

    
  set-current-plot "Tolerance"
  set-current-plot-pen "tolerance-for-others"
  set-histogram-num-bars 100
  histogram [tolerance] of turtles

  set-current-plot "nearby"
  set-current-plot-pen "sim-nearby"
  set-histogram-num-bars 100
  histogram [sim-ratio] of turtles

  
end


; Copyright 1997 Uri Wilensky.
; See Info tab for full copyright and license.
@#$#@#$#@
GRAPHICS-WINDOW
246
12
683
470
20
20
10.415
1
10
1
1
1
0
0
0
1
-20
20
-20
20
1
1
1
months
20.0

SLIDER
4
45
242
78
start-pop
start-pop
1200
6000
1780
10
1
NIL
HORIZONTAL

SLIDER
3
350
241
383
tolerance-for-others
tolerance-for-others
0
100
49
1
1
%
HORIZONTAL

BUTTON
4
10
84
43
setup
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

BUTTON
85
10
165
43
go
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
4
80
242
113
%PopHighCreative
%PopHighCreative
0
50
15
1
1
NIL
HORIZONTAL

MONITOR
893
486
969
531
% Unsatisfied
count turtles with ([satisfied? = 0]) / (count turtles) * 100
2
1
11

MONITOR
687
13
789
58
Gross City Income
sum [income] of turtles
0
1
11

SLIDER
4
114
242
147
Creative-dens
Creative-dens
1
10
3
1
1
NIL
HORIZONTAL

PLOT
688
59
890
179
Income Distribution
Income
No.
0.0
1000000.0
0.0
10.0
true
false
"" ""
PENS
"Income-dist" 1.0 1 -16777216 true "" ""
"income-cr" 1.0 1 -5825686 true "" ""

SWITCH
3
315
241
348
segregation
segregation
1
1
-1000

SLIDER
1
502
241
535
avg-rent
avg-rent
6000
350000
6000
1000
1
annual
HORIZONTAL

MONITOR
1291
386
1387
431
% Similar Nearby
sum [similar-nearby] of turtles / sum [total-nearby] of turtles * 100
2
1
11

MONITOR
789
13
862
58
Per Capita
mean [income] of turtles
0
1
11

MONITOR
864
13
950
58
Median Income
median [income] of turtles
0
1
11

TEXTBOX
4
384
181
409
Annual Income (Rs.)
12
0.0
1

SLIDER
1
434
115
467
top10%
top10%
100000
900000
254000
1000
1
NIL
HORIZONTAL

SLIDER
1
399
115
432
percapita
percapita
0
350000
42000
1000
1
NIL
HORIZONTAL

SLIDER
3
268
242
301
%brain-drain
%brain-drain
-10
30
0
.5
1
%yr
HORIZONTAL

SLIDER
4
182
242
215
pop-growth-rate
pop-growth-rate
-10
10
0
.1
1
%yr
HORIZONTAL

MONITOR
952
13
1018
58
Population
count turtles
0
1
11

SLIDER
3
233
242
266
%educated
%educated
0
100
50
1
1
college
HORIZONTAL

PLOT
689
425
891
545
Brain Drain
Time
%
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"Educated" 1.0 0 -12895429 true "" "plot percent-educated"
"Educ-Cr" 1.0 0 -5825686 true "" "plot percent-educated-cr"

MONITOR
893
438
963
483
% Educated
count turtles with [educated = 1] / count turtles * 100
1
1
11

MONITOR
1020
13
1094
58
%Hi Creative
count turtles with [creative-h = 1] / count turtles * 100
0
1
11

PLOT
893
180
1198
302
Income Structure
Time
%
0.0
10.0
0.0
100.0
true
true
"" ""
PENS
"Low" 1.0 0 -16777216 true "" "plot percent-poor"
"Middle" 1.0 0 -13345367 true "" "plot percent-middle"
"High" 1.0 0 -2674135 true "" "plot percent-rich"

PLOT
893
59
1071
179
Lorenz Curve
Pop%
Wealth%
0.0
100.0
0.0
100.0
false
true
"" ""
PENS
"Lorenz" 1.0 0 -2674135 true "" ";;if ticks > 1 [plot-pen-reset"
"Equal" 100.0 0 -16777216 true ";; draw a straight line from lower left to upper right\nset-current-plot-pen \"equal\"\nplot 0\nplot 100" ""

MONITOR
1007
117
1057
162
GINI
indexofgini
3
1
11

BUTTON
168
10
242
43
Step
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
247
486
487
519
restrict-movement-to-neighborhood
restrict-movement-to-neighborhood
0
1
-1000

CHOOSER
247
524
340
569
_Display
_Display
"Landuse" "Neighborhoods" "Rents" "none" "creative potential"
1

CHOOSER
342
523
480
568
_Turtle_Color
_Turtle_Color
"Creative" "by Neighborhood" "Income" "black" "hide" "afford rent" "Tolerance" "Content"
0

SWITCH
488
522
686
555
show-creative-space
show-creative-space
1
1
-1000

PLOT
893
303
1198
423
Creativity Structure
Time
No.
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"High" 1.0 0 -5825686 true "" "plot count turtles with [creative-h = 1]"
"Medium" 1.0 0 -2382653 true "" "plot count turtles with [creative-m = 1]"
"Low" 1.0 0 -15302303 true "" "plot count turtles with [creative-l = 1]"

TEXTBOX
4
300
169
318
Segregation and Tolerance
12
0.0
1

MONITOR
1133
376
1196
421
% Creative
count turtles with [creative = 1] / count turtles * 100
0
1
11

MONITOR
1095
13
1157
58
Celebrities
count turtles with [celebrity-status >= 3]
0
1
11

SWITCH
488
486
686
519
allow-development
allow-development
1
1
-1000

MONITOR
1075
78
1150
123
% Residential
count patches with [(landuse = 1 or landuse = 4) and outside = 0] / count patches with [outside = 0] * 100
0
1
11

TEXTBOX
1075
59
1175
77
Land Use in City
12
0.0
1

MONITOR
1152
127
1236
172
% Developable
count patches with [landuse = 0 or outside = 1] / count patches * 100
0
1
11

SLIDER
1
468
241
501
rent%-of-income
rent%-of-income
1
200
48
1
1
poverty
HORIZONTAL

CHOOSER
118
411
210
456
income-dist
income-dist
"gamma" "bi-modal"
0

PLOT
688
180
891
300
Rent Distribution
Rent
No.
1.0
350000.0
0.0
50.0
false
true
"" ""
PENS
"Overall" 1.0 1 -16777216 true "" ""
"Creative" 1.0 1 -5825686 true "" ""

MONITOR
1075
127
1151
172
% Hi Density
count patches with [landuse = 4 and outside = 0] / count patches with [outside = 0] * 100
1
1
11

PLOT
688
303
891
423
Creative Space
Time
%
0.0
100.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -5825686 true "" "plot cr-space%"

MONITOR
1150
78
1247
123
% Creative Space
count patches with [creative-space > 0] / 1681 * 100
1
1
11

SLIDER
4
148
242
181
hi-dense-level
hi-dense-level
1
5
5
1
1
NIL
HORIZONTAL

MONITOR
972
486
1062
531
% Hi Cr Income
sum ([income] of turtles with [creative-h = 1])/(sum [income] of turtles) * 100
1
1
11

TEXTBOX
6
217
156
235
Education
12
0.0
1

TEXTBOX
248
469
415
489
Spatial Environment
12
0.0
1

TEXTBOX
893
423
1060
443
Other Agent Attributes
12
0.0
1

MONITOR
970
437
1050
482
Rent-Afford
count turtles with [afford-rent = 1] / count turtles * 100
2
1
11

PLOT
1257
10
1417
130
Tolerance
Tol Level
No. 
0.0
100.0
0.0
100.0
true
false
"" ""
PENS
"tolerance-for-others" 1.0 1 -11881837 true "" ""

MONITOR
1279
436
1406
481
content-w-neighbors
count turtles with [content-w-neighbor = 1]
17
1
11

PLOT
1258
260
1418
380
nearby
sim nearby
count
0.0
100.0
0.0
10.0
true
false
"" ""
PENS
"sim-nearby" 1.0 1 -16777216 true "" ""

PLOT
1257
136
1417
256
tol-var
var
count
0.0
50.0
0.0
10.0
true
false
"" ""
PENS
"tol-var" 1.0 1 -16777216 true "" ""

@#$#@#$#@
## WHAT IS IT?

This project models the interactions of creative agents with their environment and with each other in a random city. Each agent moves around in hopes of finding a place to be satistifed to live there. As time goes on, agents may partner with other agents and sometimes get inspired to become more creative or to be entrepreneurial. Depending on the circumstances, creative clusters within the city may emerge.

This project was inspired by the writings of Richard Florida and Edward Glaeser about the Creative Class.

## HOW TO USE IT

Click the SETUP button to establish the environment. The initial display of this random city is the landuse categories typical of many cities: residential, dense residential, commercial, green space, water, gray areas, and undeveloped. The residential landuse category is the starting point for these low, medium, and high creative turtles that exist in the sapce.

The turtles may move about the city if they don’t like their current location. Their satisfaction is based on the affordability of the rent, occupancy, and perhaps the similarity of its neighbors.

The monitor for % residential land use shows the amount of residential land within the city limits. For most cities residential landuse may account for between 20 and 40% of the total area of a city. Click the setup button to see how the % changes as well as the display and distribution of agents across the model-scape.

The monitor for % creative space shows the % of land in the model that meets the creative-density threshold to be considered a creative space, which is then colored magenta.

Prior to set up, the user may specify a starting population, % of the population that is high creative to start, creative density threshold of a space, and lastly the income distribution to apply income across the population as either gamma or bimodal distribution. All other sliderse may be adjusted during the model run.


## THINGS TO NOTICE

Upon setup, large areas with no agents may represent undeveloped or areas outside the city limits, but who already have a planned landuse assigned or that may represent the most likely landuse of that space in the future.

Choosing an income-distribution of gamma will skew the income of the population to be very unequal and also very poor. This poor population can not afford rent and are very unsatisfied so as a result they keep moving. With the bi-modal distribution, the population has a very large middle-class and may be less unhappy with their current location and may choose not to move. Varying the percapita and top10%, the avg-rent, and rent as percent of income will have the greatest impact on the model as currently designed.

Over time, creative spaces may emerge into creative clusters. When a patch reaches the maximum creative value based on density of creative turtles located there or that visit there, will result in the neighboring patches to be considered as creative spaces as well with the potential to gain creative value.
As turtles move around and partner, especially in creative spaces, “celebrities” emerge that have many connections and partners.

## THINGS TO TRY

Vary the population growth rate and brain-drain to observe their impacts on the model.
Do changing the toggles for segregation, restriction of movement, and enabling development have great impact on the model’s end result?

To analyze the environment and the turtles, change the visualization features at the bottom of the model and step through the model run one step at a time.

## EXTENDING THE MODEL

Better model land market of rents and income.

Incorporate social networks into this model to see who partners.

Load in a background environment such as GIS data to change the background display.
Add in the R extension to perform spatial correlation and statistics in the NetLogo environment.

## NETLOGO FEATURES

n-of and sprout are used to create turtles while ensuring no patch has more than one turtle on it.

When a turtle moves, move-to is used to move the turtle to the center of the patch it eventually finds.

## CREDITS AND REFERENCES

Economist, The. 2010. “Economics Focus: Agents of Change.” The Economist, July 22. http://www.economist.com/node/16636121.

Florida, Richard. 2002. “The Rise of the Creative Class.” Washington Monthly (May). http://www.washingtonmonthly.com/features/2001/0205.florida.html. 

———. 2012. The Rise of the Creative Class–Revisited: 10th Anniversary Edition–Revised and Expanded. Second ed. Basic Books.

Glaeser, Edward L. 2011. Triumph of the City: How Our Greatest Invention Makes Us Richer, Smarter, Greener, Healthier, and Happier. First ed. Penguin Press HC, The

## HOW TO CITE

If you mention this model in a publication, we ask that you include these citations for the model itself and for the NetLogo software:

The authors of this model are George Mason University students Ammar Malik and Melanie Swartz.

This NetLogo Creative City model was created as part of a research project to explore the use of agent based modeling simulations for solving urban challenges. This particular simulation is attempting to model the formation of creative clusters to explore the role of creativity and economic development.

For Netlogo software:

Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

## COPYRIGHT AND LICENSE

Copyright 1997 Uri Wilensky.
 
This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License. To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.

Commercial licenses are also available. To inquire about commercial licenses, please contact Uri Wilensky at uri@northwestern.edu.
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

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.0.3
@#$#@#$#@
@#$#@#$#@
1.0
    org.nlogo.sdm.gui.AggregateDrawing 2
        org.nlogo.sdm.gui.StockFigure "attributes" "attributes" 1 "FillColor" "Color" 225 225 182 129 111 60 40
            org.nlogo.sdm.gui.WrappedStock "" "" 0
        org.nlogo.sdm.gui.ConverterFigure "attributes" "attributes" 1 "FillColor" "Color" 130 188 183 156 227 50 50
            org.nlogo.sdm.gui.WrappedConverter "" ""
@#$#@#$#@
<experiments>
  <experiment name="Trial 1" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="120"/>
    <metric>count turtles with [creative = 1] / count turtles * 100</metric>
    <metric>mean [income] of turtles</metric>
    <metric>sum ([income] of turtles with [creative-h = 1])/(sum [income] of turtles) * 100</metric>
    <enumeratedValueSet variable="%PopHighCreative">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance-for-others">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-rent">
      <value value="745"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rent%-of-income">
      <value value="55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-creative-space">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restrict-movement-to-neighborhood">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-dist">
      <value value="&quot;gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-development">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-growth-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%educated">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%brain-drain">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hi-dense-level">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Creative-dens">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-pop">
      <value value="2520"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percapita">
      <value value="33400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Turtle_color">
      <value value="&quot;Creative&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segregation">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Display">
      <value value="&quot;Landuse&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="top10%">
      <value value="96000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="5 Year Default" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="60"/>
    <metric>count turtles with [creative-h = 1] / count turtles * 100</metric>
    <metric>count turtles with [creative = 1] / count turtles * 100</metric>
    <metric>indexofgini</metric>
    <metric>count patches with [creative-space &gt; 0] / 1681 * 100</metric>
    <metric>sum ([income] of turtles with [creative-h = 1])/(sum [income] of turtles) * 100</metric>
    <metric>mean [income] of turtles</metric>
    <metric>count turtles with [afford-rent = 1] / count turtles * 100</metric>
    <enumeratedValueSet variable="%PopHighCreative">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance-for-others">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-rent">
      <value value="72000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rent%-of-income">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-creative-space">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restrict-movement-to-neighborhood">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-dist">
      <value value="&quot;gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-development">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-growth-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%educated">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%brain-drain">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hi-dense-level">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Creative-dens">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-pop">
      <value value="1800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percapita">
      <value value="120000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Turtle_color">
      <value value="&quot;Creative&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segregation">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Display">
      <value value="&quot;Landuse&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="top10%">
      <value value="350000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="1 Year Default" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="12"/>
    <metric>count turtles with [creative-h = 1] / count turtles * 100</metric>
    <metric>count turtles with [creative = 1] / count turtles * 100</metric>
    <metric>indexofgini</metric>
    <metric>count patches with [creative-space &gt; 0] / 1681 * 100</metric>
    <metric>sum ([income] of turtles with [creative-h = 1])/(sum [income] of turtles) * 100</metric>
    <metric>mean [income] of turtles</metric>
    <metric>count turtles with [afford-rent = 1] / count turtles * 100</metric>
    <enumeratedValueSet variable="%PopHighCreative">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance-for-others">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-rent">
      <value value="72000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rent%-of-income">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-creative-space">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restrict-movement-to-neighborhood">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-dist">
      <value value="&quot;gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-development">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-growth-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%educated">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%brain-drain">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hi-dense-level">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Creative-dens">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-pop">
      <value value="1800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percapita">
      <value value="120000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Turtle_color">
      <value value="&quot;Creative&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segregation">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Display">
      <value value="&quot;Landuse&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="top10%">
      <value value="350000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="10 Year Default" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="120"/>
    <metric>count turtles with [creative-h = 1] / count turtles * 100</metric>
    <metric>count turtles with [creative = 1] / count turtles * 100</metric>
    <metric>indexofgini</metric>
    <metric>count patches with [creative-space &gt; 0] / 1681 * 100</metric>
    <metric>sum ([income] of turtles with [creative-h = 1])/(sum [income] of turtles) * 100</metric>
    <metric>mean [income] of turtles</metric>
    <metric>count turtles with [afford-rent = 1] / count turtles * 100</metric>
    <enumeratedValueSet variable="%PopHighCreative">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance-for-others">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-rent">
      <value value="72000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rent%-of-income">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-creative-space">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restrict-movement-to-neighborhood">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-dist">
      <value value="&quot;gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-development">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-growth-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%educated">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%brain-drain">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hi-dense-level">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Creative-dens">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-pop">
      <value value="1800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percapita">
      <value value="120000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Turtle_color">
      <value value="&quot;Creative&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segregation">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Display">
      <value value="&quot;Landuse&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="top10%">
      <value value="350000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="15 Year Default" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="180"/>
    <metric>count turtles with [creative-h = 1] / count turtles * 100</metric>
    <metric>count turtles with [creative = 1] / count turtles * 100</metric>
    <metric>indexofgini</metric>
    <metric>count patches with [creative-space &gt; 0] / 1681 * 100</metric>
    <metric>sum ([income] of turtles with [creative-h = 1])/(sum [income] of turtles) * 100</metric>
    <metric>mean [income] of turtles</metric>
    <metric>count turtles with [afford-rent = 1] / count turtles * 100</metric>
    <enumeratedValueSet variable="%PopHighCreative">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance-for-others">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-rent">
      <value value="72000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rent%-of-income">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-creative-space">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restrict-movement-to-neighborhood">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-dist">
      <value value="&quot;gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-development">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-growth-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%educated">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%brain-drain">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hi-dense-level">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Creative-dens">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-pop">
      <value value="1800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percapita">
      <value value="120000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Turtle_color">
      <value value="&quot;Creative&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segregation">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Display">
      <value value="&quot;Landuse&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="top10%">
      <value value="350000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20 Year Default" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="240"/>
    <metric>count turtles with [creative-h = 1] / count turtles * 100</metric>
    <metric>count turtles with [creative = 1] / count turtles * 100</metric>
    <metric>indexofgini</metric>
    <metric>count patches with [creative-space &gt; 0] / 1681 * 100</metric>
    <metric>sum ([income] of turtles with [creative-h = 1])/(sum [income] of turtles) * 100</metric>
    <metric>mean [income] of turtles</metric>
    <metric>count turtles with [afford-rent = 1] / count turtles * 100</metric>
    <enumeratedValueSet variable="%PopHighCreative">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance-for-others">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-rent">
      <value value="72000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rent%-of-income">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-creative-space">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restrict-movement-to-neighborhood">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-dist">
      <value value="&quot;gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-development">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-growth-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%educated">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%brain-drain">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hi-dense-level">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Creative-dens">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-pop">
      <value value="1800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percapita">
      <value value="120000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Turtle_color">
      <value value="&quot;Creative&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segregation">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Display">
      <value value="&quot;Landuse&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="top10%">
      <value value="350000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Hi Tolerance 5% similarity acceptable 1 yr" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="12"/>
    <metric>count turtles with [creative-h = 1] / count turtles * 100</metric>
    <metric>count turtles with [creative = 1] / count turtles * 100</metric>
    <metric>indexofgini</metric>
    <metric>count patches with [creative-space &gt; 0] / 1681 * 100</metric>
    <metric>count turtles with [educated = 1] / count turtles * 100</metric>
    <metric>sum ([income] of turtles with [creative-h = 1])/(sum [income] of turtles) * 100</metric>
    <metric>mean [income] of turtles</metric>
    <metric>count turtles with [afford-rent = 1] / count turtles * 100</metric>
    <enumeratedValueSet variable="%PopHighCreative">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance-for-others">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-rent">
      <value value="72000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rent%-of-income">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-creative-space">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restrict-movement-to-neighborhood">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-dist">
      <value value="&quot;gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-development">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-growth-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%educated">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%brain-drain">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hi-dense-level">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Creative-dens">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-pop">
      <value value="1800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percapita">
      <value value="119000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Turtle_color">
      <value value="&quot;Creative&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segregation">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Display">
      <value value="&quot;Landuse&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="top10%">
      <value value="500000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Hi Tolerance 5% similarity acceptable 5 ry" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="60"/>
    <metric>count turtles with [creative-h = 1] / count turtles * 100</metric>
    <metric>count turtles with [creative = 1] / count turtles * 100</metric>
    <metric>indexofgini</metric>
    <metric>count patches with [creative-space &gt; 0] / 1681 * 100</metric>
    <metric>count turtles with [educated = 1] / count turtles * 100</metric>
    <metric>sum ([income] of turtles with [creative-h = 1])/(sum [income] of turtles) * 100</metric>
    <metric>mean [income] of turtles</metric>
    <metric>count turtles with [afford-rent = 1] / count turtles * 100</metric>
    <enumeratedValueSet variable="%PopHighCreative">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance-for-others">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-rent">
      <value value="72000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rent%-of-income">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-creative-space">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restrict-movement-to-neighborhood">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-dist">
      <value value="&quot;gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-development">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-growth-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%educated">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%brain-drain">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hi-dense-level">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Creative-dens">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-pop">
      <value value="1800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percapita">
      <value value="119000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Turtle_color">
      <value value="&quot;Creative&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segregation">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Display">
      <value value="&quot;Landuse&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="top10%">
      <value value="500000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Hi Tolerance 5% similarity acceptable 10 yr" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="120"/>
    <metric>count turtles with [creative-h = 1] / count turtles * 100</metric>
    <metric>count turtles with [creative = 1] / count turtles * 100</metric>
    <metric>indexofgini</metric>
    <metric>count patches with [creative-space &gt; 0] / 1681 * 100</metric>
    <metric>count turtles with [educated = 1] / count turtles * 100</metric>
    <metric>sum ([income] of turtles with [creative-h = 1])/(sum [income] of turtles) * 100</metric>
    <metric>mean [income] of turtles</metric>
    <metric>count turtles with [afford-rent = 1] / count turtles * 100</metric>
    <enumeratedValueSet variable="%PopHighCreative">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance-for-others">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-rent">
      <value value="72000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rent%-of-income">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-creative-space">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restrict-movement-to-neighborhood">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-dist">
      <value value="&quot;gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-development">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-growth-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%educated">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%brain-drain">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hi-dense-level">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Creative-dens">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-pop">
      <value value="1800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percapita">
      <value value="119000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Turtle_color">
      <value value="&quot;Creative&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segregation">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Display">
      <value value="&quot;Landuse&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="top10%">
      <value value="500000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Hi Tolerance 5% similarity acceptable 15 yr" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="180"/>
    <metric>count turtles with [creative-h = 1] / count turtles * 100</metric>
    <metric>count turtles with [creative = 1] / count turtles * 100</metric>
    <metric>indexofgini</metric>
    <metric>count patches with [creative-space &gt; 0] / 1681 * 100</metric>
    <metric>count turtles with [educated = 1] / count turtles * 100</metric>
    <metric>sum ([income] of turtles with [creative-h = 1])/(sum [income] of turtles) * 100</metric>
    <metric>mean [income] of turtles</metric>
    <metric>count turtles with [afford-rent = 1] / count turtles * 100</metric>
    <enumeratedValueSet variable="%PopHighCreative">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance-for-others">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-rent">
      <value value="72000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rent%-of-income">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-creative-space">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restrict-movement-to-neighborhood">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-dist">
      <value value="&quot;gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-development">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-growth-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%educated">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%brain-drain">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hi-dense-level">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Creative-dens">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-pop">
      <value value="1800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percapita">
      <value value="119000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Turtle_color">
      <value value="&quot;Creative&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segregation">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Display">
      <value value="&quot;Landuse&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="top10%">
      <value value="500000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Hi Tolerance 5% similarity acceptable 20 yr" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="240"/>
    <metric>count turtles with [creative-h = 1] / count turtles * 100</metric>
    <metric>count turtles with [creative = 1] / count turtles * 100</metric>
    <metric>indexofgini</metric>
    <metric>count patches with [creative-space &gt; 0] / 1681 * 100</metric>
    <metric>count turtles with [educated = 1] / count turtles * 100</metric>
    <metric>sum ([income] of turtles with [creative-h = 1])/(sum [income] of turtles) * 100</metric>
    <metric>mean [income] of turtles</metric>
    <metric>count turtles with [afford-rent = 1] / count turtles * 100</metric>
    <enumeratedValueSet variable="%PopHighCreative">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance-for-others">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-rent">
      <value value="72000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rent%-of-income">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-creative-space">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restrict-movement-to-neighborhood">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-dist">
      <value value="&quot;gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-development">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-growth-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%educated">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%brain-drain">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hi-dense-level">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Creative-dens">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-pop">
      <value value="1800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percapita">
      <value value="119000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Turtle_color">
      <value value="&quot;Creative&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segregation">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Display">
      <value value="&quot;Landuse&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="top10%">
      <value value="500000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Hi Brain Drain @ 50% 1 yr" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="12"/>
    <metric>count turtles with [creative-h = 1] / count turtles * 100</metric>
    <metric>count turtles with [creative = 1] / count turtles * 100</metric>
    <metric>indexofgini</metric>
    <metric>count patches with [creative-space &gt; 0] / 1681 * 100</metric>
    <metric>count turtles with [educated = 1] / count turtles * 100</metric>
    <metric>sum ([income] of turtles with [creative-h = 1])/(sum [income] of turtles) * 100</metric>
    <metric>mean [income] of turtles</metric>
    <metric>count turtles with [afford-rent = 1] / count turtles * 100</metric>
    <enumeratedValueSet variable="%PopHighCreative">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance-for-others">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-rent">
      <value value="72000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rent%-of-income">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-creative-space">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restrict-movement-to-neighborhood">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-dist">
      <value value="&quot;gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-development">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-growth-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%educated">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%brain-drain">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hi-dense-level">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Creative-dens">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-pop">
      <value value="1800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percapita">
      <value value="119000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Turtle_color">
      <value value="&quot;Creative&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segregation">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Display">
      <value value="&quot;Landuse&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="top10%">
      <value value="500000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Hi Brain Drain @ 50% 5 yr" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="60"/>
    <metric>count turtles with [creative-h = 1] / count turtles * 100</metric>
    <metric>count turtles with [creative = 1] / count turtles * 100</metric>
    <metric>indexofgini</metric>
    <metric>count patches with [creative-space &gt; 0] / 1681 * 100</metric>
    <metric>count turtles with [educated = 1] / count turtles * 100</metric>
    <metric>sum ([income] of turtles with [creative-h = 1])/(sum [income] of turtles) * 100</metric>
    <metric>mean [income] of turtles</metric>
    <metric>count turtles with [afford-rent = 1] / count turtles * 100</metric>
    <enumeratedValueSet variable="%PopHighCreative">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance-for-others">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-rent">
      <value value="72000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rent%-of-income">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-creative-space">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restrict-movement-to-neighborhood">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-dist">
      <value value="&quot;gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-development">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-growth-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%educated">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%brain-drain">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hi-dense-level">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Creative-dens">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-pop">
      <value value="1800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percapita">
      <value value="119000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Turtle_color">
      <value value="&quot;Creative&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segregation">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Display">
      <value value="&quot;Landuse&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="top10%">
      <value value="500000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Hi Brain Drain @ 50% 10 yr" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="120"/>
    <metric>count turtles with [creative-h = 1] / count turtles * 100</metric>
    <metric>count turtles with [creative = 1] / count turtles * 100</metric>
    <metric>indexofgini</metric>
    <metric>count patches with [creative-space &gt; 0] / 1681 * 100</metric>
    <metric>count turtles with [educated = 1] / count turtles * 100</metric>
    <metric>sum ([income] of turtles with [creative-h = 1])/(sum [income] of turtles) * 100</metric>
    <metric>mean [income] of turtles</metric>
    <metric>count turtles with [afford-rent = 1] / count turtles * 100</metric>
    <enumeratedValueSet variable="%PopHighCreative">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance-for-others">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-rent">
      <value value="72000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rent%-of-income">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-creative-space">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restrict-movement-to-neighborhood">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-dist">
      <value value="&quot;gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-development">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-growth-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%educated">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%brain-drain">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hi-dense-level">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Creative-dens">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-pop">
      <value value="1800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percapita">
      <value value="119000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Turtle_color">
      <value value="&quot;Creative&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segregation">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Display">
      <value value="&quot;Landuse&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="top10%">
      <value value="500000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Hi Brain Drain @ 50% 15 yr" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="180"/>
    <metric>count turtles with [creative-h = 1] / count turtles * 100</metric>
    <metric>count turtles with [creative = 1] / count turtles * 100</metric>
    <metric>indexofgini</metric>
    <metric>count patches with [creative-space &gt; 0] / 1681 * 100</metric>
    <metric>count turtles with [educated = 1] / count turtles * 100</metric>
    <metric>sum ([income] of turtles with [creative-h = 1])/(sum [income] of turtles) * 100</metric>
    <metric>mean [income] of turtles</metric>
    <metric>count turtles with [afford-rent = 1] / count turtles * 100</metric>
    <enumeratedValueSet variable="%PopHighCreative">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance-for-others">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-rent">
      <value value="72000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rent%-of-income">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-creative-space">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restrict-movement-to-neighborhood">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-dist">
      <value value="&quot;gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-development">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-growth-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%educated">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%brain-drain">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hi-dense-level">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Creative-dens">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-pop">
      <value value="1800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percapita">
      <value value="119000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Turtle_color">
      <value value="&quot;Creative&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segregation">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Display">
      <value value="&quot;Landuse&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="top10%">
      <value value="500000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Hi Brain Drain @ 50% 20 yr" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="240"/>
    <metric>count turtles with [creative-h = 1] / count turtles * 100</metric>
    <metric>count turtles with [creative = 1] / count turtles * 100</metric>
    <metric>indexofgini</metric>
    <metric>count patches with [creative-space &gt; 0] / 1681 * 100</metric>
    <metric>count turtles with [educated = 1] / count turtles * 100</metric>
    <metric>sum ([income] of turtles with [creative-h = 1])/(sum [income] of turtles) * 100</metric>
    <metric>mean [income] of turtles</metric>
    <metric>count turtles with [afford-rent = 1] / count turtles * 100</metric>
    <enumeratedValueSet variable="%PopHighCreative">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance-for-others">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-rent">
      <value value="72000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rent%-of-income">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-creative-space">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restrict-movement-to-neighborhood">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-dist">
      <value value="&quot;gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-development">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-growth-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%educated">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%brain-drain">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hi-dense-level">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Creative-dens">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-pop">
      <value value="1800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percapita">
      <value value="119000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Turtle_color">
      <value value="&quot;Creative&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segregation">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Display">
      <value value="&quot;Landuse&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="top10%">
      <value value="500000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Hi Univ Educ @ 90% 1 yr" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="12"/>
    <metric>count turtles with [creative-h = 1] / count turtles * 100</metric>
    <metric>count turtles with [creative = 1] / count turtles * 100</metric>
    <metric>indexofgini</metric>
    <metric>count patches with [creative-space &gt; 0] / 1681 * 100</metric>
    <metric>count turtles with [educated = 1] / count turtles * 100</metric>
    <metric>sum ([income] of turtles with [creative-h = 1])/(sum [income] of turtles) * 100</metric>
    <metric>mean [income] of turtles</metric>
    <metric>count turtles with [afford-rent = 1] / count turtles * 100</metric>
    <enumeratedValueSet variable="%PopHighCreative">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance-for-others">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-rent">
      <value value="72000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rent%-of-income">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-creative-space">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restrict-movement-to-neighborhood">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-dist">
      <value value="&quot;gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-development">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-growth-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%educated">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%brain-drain">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hi-dense-level">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Creative-dens">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-pop">
      <value value="1800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percapita">
      <value value="119000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Turtle_color">
      <value value="&quot;Creative&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segregation">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Display">
      <value value="&quot;Landuse&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="top10%">
      <value value="500000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Hi Univ Educ @ 90% 5 yr" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="60"/>
    <metric>count turtles with [creative-h = 1] / count turtles * 100</metric>
    <metric>count turtles with [creative = 1] / count turtles * 100</metric>
    <metric>indexofgini</metric>
    <metric>count patches with [creative-space &gt; 0] / 1681 * 100</metric>
    <metric>count turtles with [educated = 1] / count turtles * 100</metric>
    <metric>sum ([income] of turtles with [creative-h = 1])/(sum [income] of turtles) * 100</metric>
    <metric>mean [income] of turtles</metric>
    <metric>count turtles with [afford-rent = 1] / count turtles * 100</metric>
    <enumeratedValueSet variable="%PopHighCreative">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance-for-others">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-rent">
      <value value="72000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rent%-of-income">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-creative-space">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restrict-movement-to-neighborhood">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-dist">
      <value value="&quot;gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-development">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-growth-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%educated">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%brain-drain">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hi-dense-level">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Creative-dens">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-pop">
      <value value="1800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percapita">
      <value value="119000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Turtle_color">
      <value value="&quot;Creative&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segregation">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Display">
      <value value="&quot;Landuse&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="top10%">
      <value value="500000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Hi Univ Educ @ 90% 10 yr" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="120"/>
    <metric>count turtles with [creative-h = 1] / count turtles * 100</metric>
    <metric>count turtles with [creative = 1] / count turtles * 100</metric>
    <metric>indexofgini</metric>
    <metric>count patches with [creative-space &gt; 0] / 1681 * 100</metric>
    <metric>count turtles with [educated = 1] / count turtles * 100</metric>
    <metric>sum ([income] of turtles with [creative-h = 1])/(sum [income] of turtles) * 100</metric>
    <metric>mean [income] of turtles</metric>
    <metric>count turtles with [afford-rent = 1] / count turtles * 100</metric>
    <enumeratedValueSet variable="%PopHighCreative">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance-for-others">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-rent">
      <value value="72000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rent%-of-income">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-creative-space">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restrict-movement-to-neighborhood">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-dist">
      <value value="&quot;gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-development">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-growth-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%educated">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%brain-drain">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hi-dense-level">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Creative-dens">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-pop">
      <value value="1800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percapita">
      <value value="119000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Turtle_color">
      <value value="&quot;Creative&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segregation">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Display">
      <value value="&quot;Landuse&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="top10%">
      <value value="500000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Hi Univ Educ @ 90% 15 yr" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="180"/>
    <metric>count turtles with [creative-h = 1] / count turtles * 100</metric>
    <metric>count turtles with [creative = 1] / count turtles * 100</metric>
    <metric>indexofgini</metric>
    <metric>count patches with [creative-space &gt; 0] / 1681 * 100</metric>
    <metric>count turtles with [educated = 1] / count turtles * 100</metric>
    <metric>sum ([income] of turtles with [creative-h = 1])/(sum [income] of turtles) * 100</metric>
    <metric>mean [income] of turtles</metric>
    <metric>count turtles with [afford-rent = 1] / count turtles * 100</metric>
    <enumeratedValueSet variable="%PopHighCreative">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance-for-others">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-rent">
      <value value="72000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rent%-of-income">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-creative-space">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restrict-movement-to-neighborhood">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-dist">
      <value value="&quot;gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-development">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-growth-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%educated">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%brain-drain">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hi-dense-level">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Creative-dens">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-pop">
      <value value="1800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percapita">
      <value value="119000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Turtle_color">
      <value value="&quot;Creative&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segregation">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Display">
      <value value="&quot;Landuse&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="top10%">
      <value value="500000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Low Tolerance @ 75% similarity wanted 1 yr" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="12"/>
    <metric>count turtles with [creative-h = 1] / count turtles * 100</metric>
    <metric>count turtles with [creative = 1] / count turtles * 100</metric>
    <metric>indexofgini</metric>
    <metric>count patches with [creative-space &gt; 0] / 1681 * 100</metric>
    <metric>count turtles with [educated = 1] / count turtles * 100</metric>
    <metric>sum ([income] of turtles with [creative-h = 1])/(sum [income] of turtles) * 100</metric>
    <metric>mean [income] of turtles</metric>
    <metric>count turtles with [afford-rent = 1] / count turtles * 100</metric>
    <enumeratedValueSet variable="%PopHighCreative">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance-for-others">
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-rent">
      <value value="72000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rent%-of-income">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-creative-space">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restrict-movement-to-neighborhood">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-dist">
      <value value="&quot;gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-development">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-growth-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%educated">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%brain-drain">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hi-dense-level">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Creative-dens">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-pop">
      <value value="1800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percapita">
      <value value="119000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Turtle_color">
      <value value="&quot;Creative&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segregation">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Display">
      <value value="&quot;Landuse&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="top10%">
      <value value="500000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Low Tolerance @ 75% similarity wanted 5 yr" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="60"/>
    <metric>count turtles with [creative-h = 1] / count turtles * 100</metric>
    <metric>count turtles with [creative = 1] / count turtles * 100</metric>
    <metric>indexofgini</metric>
    <metric>count patches with [creative-space &gt; 0] / 1681 * 100</metric>
    <metric>count turtles with [educated = 1] / count turtles * 100</metric>
    <metric>sum ([income] of turtles with [creative-h = 1])/(sum [income] of turtles) * 100</metric>
    <metric>mean [income] of turtles</metric>
    <metric>count turtles with [afford-rent = 1] / count turtles * 100</metric>
    <enumeratedValueSet variable="%PopHighCreative">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance-for-others">
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-rent">
      <value value="72000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rent%-of-income">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-creative-space">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restrict-movement-to-neighborhood">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-dist">
      <value value="&quot;gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-development">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-growth-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%educated">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%brain-drain">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hi-dense-level">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Creative-dens">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-pop">
      <value value="1800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percapita">
      <value value="119000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Turtle_color">
      <value value="&quot;Creative&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segregation">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Display">
      <value value="&quot;Landuse&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="top10%">
      <value value="500000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Low Tolerance @ 75% similarity wanted 10 yr" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="120"/>
    <metric>count turtles with [creative-h = 1] / count turtles * 100</metric>
    <metric>count turtles with [creative = 1] / count turtles * 100</metric>
    <metric>indexofgini</metric>
    <metric>count patches with [creative-space &gt; 0] / 1681 * 100</metric>
    <metric>count turtles with [educated = 1] / count turtles * 100</metric>
    <metric>sum ([income] of turtles with [creative-h = 1])/(sum [income] of turtles) * 100</metric>
    <metric>mean [income] of turtles</metric>
    <metric>count turtles with [afford-rent = 1] / count turtles * 100</metric>
    <enumeratedValueSet variable="%PopHighCreative">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance-for-others">
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-rent">
      <value value="72000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rent%-of-income">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-creative-space">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restrict-movement-to-neighborhood">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-dist">
      <value value="&quot;gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-development">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-growth-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%educated">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%brain-drain">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hi-dense-level">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Creative-dens">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-pop">
      <value value="1800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percapita">
      <value value="119000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Turtle_color">
      <value value="&quot;Creative&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segregation">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Display">
      <value value="&quot;Landuse&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="top10%">
      <value value="500000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Low Tolerance @ 75% similarity wanted 15 yr" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="180"/>
    <metric>count turtles with [creative-h = 1] / count turtles * 100</metric>
    <metric>count turtles with [creative = 1] / count turtles * 100</metric>
    <metric>indexofgini</metric>
    <metric>count patches with [creative-space &gt; 0] / 1681 * 100</metric>
    <metric>count turtles with [educated = 1] / count turtles * 100</metric>
    <metric>sum ([income] of turtles with [creative-h = 1])/(sum [income] of turtles) * 100</metric>
    <metric>mean [income] of turtles</metric>
    <metric>count turtles with [afford-rent = 1] / count turtles * 100</metric>
    <enumeratedValueSet variable="%PopHighCreative">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance-for-others">
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-rent">
      <value value="72000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rent%-of-income">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-creative-space">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restrict-movement-to-neighborhood">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-dist">
      <value value="&quot;gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-development">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-growth-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%educated">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%brain-drain">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hi-dense-level">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Creative-dens">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-pop">
      <value value="1800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percapita">
      <value value="119000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Turtle_color">
      <value value="&quot;Creative&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segregation">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Display">
      <value value="&quot;Landuse&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="top10%">
      <value value="500000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Low Tolerance @ 75% similarity wanted 20 yr" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="240"/>
    <metric>count turtles with [creative-h = 1] / count turtles * 100</metric>
    <metric>count turtles with [creative = 1] / count turtles * 100</metric>
    <metric>indexofgini</metric>
    <metric>count patches with [creative-space &gt; 0] / 1681 * 100</metric>
    <metric>count turtles with [educated = 1] / count turtles * 100</metric>
    <metric>sum ([income] of turtles with [creative-h = 1])/(sum [income] of turtles) * 100</metric>
    <metric>mean [income] of turtles</metric>
    <metric>count turtles with [afford-rent = 1] / count turtles * 100</metric>
    <enumeratedValueSet variable="%PopHighCreative">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance-for-others">
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-rent">
      <value value="72000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rent%-of-income">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-creative-space">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restrict-movement-to-neighborhood">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-dist">
      <value value="&quot;gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-development">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-growth-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%educated">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%brain-drain">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hi-dense-level">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Creative-dens">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-pop">
      <value value="1800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percapita">
      <value value="119000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Turtle_color">
      <value value="&quot;Creative&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segregation">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Display">
      <value value="&quot;Landuse&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="top10%">
      <value value="500000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Hi Univ Educ @ 90% 20 yr" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="240"/>
    <metric>count turtles with [creative-h = 1] / count turtles * 100</metric>
    <metric>count turtles with [creative = 1] / count turtles * 100</metric>
    <metric>indexofgini</metric>
    <metric>count patches with [creative-space &gt; 0] / 1681 * 100</metric>
    <metric>count turtles with [educated = 1] / count turtles * 100</metric>
    <metric>sum ([income] of turtles with [creative-h = 1])/(sum [income] of turtles) * 100</metric>
    <metric>mean [income] of turtles</metric>
    <metric>count turtles with [afford-rent = 1] / count turtles * 100</metric>
    <enumeratedValueSet variable="%PopHighCreative">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance-for-others">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-rent">
      <value value="72000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rent%-of-income">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-creative-space">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restrict-movement-to-neighborhood">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-dist">
      <value value="&quot;gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-development">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-growth-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%educated">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%brain-drain">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hi-dense-level">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Creative-dens">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-pop">
      <value value="1800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percapita">
      <value value="119000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Turtle_color">
      <value value="&quot;Creative&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segregation">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Display">
      <value value="&quot;Landuse&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="top10%">
      <value value="500000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Seg ON Mov OFF" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="120"/>
    <metric>count turtles with [creative-h = 1] / count turtles * 100</metric>
    <metric>count turtles with [creative = 1] / count turtles * 100</metric>
    <metric>indexofgini</metric>
    <metric>count patches with [creative-space &gt; 0] / 1681 * 100</metric>
    <metric>count turtles with [educated = 1] / count turtles * 100</metric>
    <metric>sum ([income] of turtles with [creative-h = 1])/(sum [income] of turtles) * 100</metric>
    <metric>mean [income] of turtles</metric>
    <metric>count turtles with [afford-rent = 1] / count turtles *</metric>
    <enumeratedValueSet variable="%PopHighCreative">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance-for-others">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-rent">
      <value value="72000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rent%-of-income">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-creative-space">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restrict-movement-to-neighborhood">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-dist">
      <value value="&quot;gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-development">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-growth-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%educated">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%brain-drain">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hi-dense-level">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Creative-dens">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-pop">
      <value value="1800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percapita">
      <value value="119000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Turtle_color">
      <value value="&quot;Creative&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segregation">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Display">
      <value value="&quot;Landuse&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="top10%">
      <value value="500000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Seg OFF Mov ON" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="240"/>
    <metric>count turtles with [creative-h = 1] / count turtles * 100</metric>
    <metric>count turtles with [creative = 1] / count turtles * 100</metric>
    <metric>indexofgini</metric>
    <metric>count patches with [creative-space &gt; 0] / 1681 * 100</metric>
    <metric>count turtles with [educated = 1] / count turtles * 100</metric>
    <metric>sum ([income] of turtles with [creative-h = 1])/(sum [income] of turtles) * 100</metric>
    <metric>mean [income] of turtles</metric>
    <metric>count turtles with [afford-rent = 1] / count turtles * 100</metric>
    <enumeratedValueSet variable="%PopHighCreative">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance-for-others">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-rent">
      <value value="72000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rent%-of-income">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-creative-space">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restrict-movement-to-neighborhood">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-dist">
      <value value="&quot;gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-development">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-growth-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%educated">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%brain-drain">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hi-dense-level">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Creative-dens">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-pop">
      <value value="1800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percapita">
      <value value="119000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Turtle_color">
      <value value="&quot;Creative&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segregation">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Display">
      <value value="&quot;Landuse&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="top10%">
      <value value="500000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Seg OFF Mov OFF" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="240"/>
    <metric>count turtles with [creative-h = 1] / count turtles * 100</metric>
    <metric>count turtles with [creative = 1] / count turtles * 100</metric>
    <metric>indexofgini</metric>
    <metric>count patches with [creative-space &gt; 0] / 1681 * 100</metric>
    <metric>count turtles with [educated = 1] / count turtles * 100</metric>
    <metric>sum ([income] of turtles with [creative-h = 1])/(sum [income] of turtles) * 100</metric>
    <metric>mean [income] of turtles</metric>
    <metric>count turtles with [afford-rent = 1] / count turtles * 100</metric>
    <enumeratedValueSet variable="%PopHighCreative">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance-for-others">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-rent">
      <value value="72000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rent%-of-income">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-creative-space">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restrict-movement-to-neighborhood">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-dist">
      <value value="&quot;gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-development">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-growth-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%educated">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%brain-drain">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hi-dense-level">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Creative-dens">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-pop">
      <value value="1800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percapita">
      <value value="119000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Turtle_color">
      <value value="&quot;Creative&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segregation">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Display">
      <value value="&quot;Landuse&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="top10%">
      <value value="500000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Seg ON Mov ON" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="240"/>
    <metric>count turtles with [creative-h = 1] / count turtles * 100</metric>
    <metric>count turtles with [creative = 1] / count turtles * 100</metric>
    <metric>indexofgini</metric>
    <metric>count patches with [creative-space &gt; 0] / 1681 * 100</metric>
    <metric>count turtles with [educated = 1] / count turtles * 100</metric>
    <metric>sum ([income] of turtles with [creative-h = 1])/(sum [income] of turtles) * 100</metric>
    <metric>mean [income] of turtles</metric>
    <metric>count turtles with [afford-rent = 1] / count turtles * 100</metric>
    <enumeratedValueSet variable="%PopHighCreative">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance-for-others">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-rent">
      <value value="72000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rent%-of-income">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-creative-space">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restrict-movement-to-neighborhood">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-dist">
      <value value="&quot;gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-development">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-growth-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%educated">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%brain-drain">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hi-dense-level">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Creative-dens">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-pop">
      <value value="1800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percapita">
      <value value="119000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Turtle_color">
      <value value="&quot;Creative&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segregation">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Display">
      <value value="&quot;Landuse&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="top10%">
      <value value="500000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Dev OFF Mov OFF" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="240"/>
    <metric>count turtles with [creative-h = 1] / count turtles * 100</metric>
    <metric>count turtles with [creative = 1] / count turtles * 100</metric>
    <metric>indexofgini</metric>
    <metric>count patches with [creative-space &gt; 0] / 1681 * 100</metric>
    <metric>count turtles with [educated = 1] / count turtles * 100</metric>
    <metric>sum ([income] of turtles with [creative-h = 1])/(sum [income] of turtles) * 100</metric>
    <metric>mean [income] of turtles</metric>
    <metric>count turtles with [afford-rent = 1] / count turtles * 100</metric>
    <enumeratedValueSet variable="%PopHighCreative">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance-for-others">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-rent">
      <value value="72000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rent%-of-income">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-creative-space">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restrict-movement-to-neighborhood">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-dist">
      <value value="&quot;gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-development">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-growth-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%educated">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%brain-drain">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hi-dense-level">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Creative-dens">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-pop">
      <value value="1800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percapita">
      <value value="119000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Turtle_color">
      <value value="&quot;Creative&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segregation">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Display">
      <value value="&quot;Landuse&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="top10%">
      <value value="500000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Dev ON Mov ON" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="240"/>
    <metric>count turtles with [creative-h = 1] / count turtles * 100</metric>
    <metric>count turtles with [creative = 1] / count turtles * 100</metric>
    <metric>indexofgini</metric>
    <metric>count patches with [creative-space &gt; 0] / 1681 * 100</metric>
    <metric>count turtles with [educated = 1] / count turtles * 100</metric>
    <metric>sum ([income] of turtles with [creative-h = 1])/(sum [income] of turtles) * 100</metric>
    <metric>mean [income] of turtles</metric>
    <metric>count turtles with [afford-rent = 1] / count turtles * 100</metric>
    <enumeratedValueSet variable="%PopHighCreative">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance-for-others">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-rent">
      <value value="72000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rent%-of-income">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-creative-space">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restrict-movement-to-neighborhood">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-dist">
      <value value="&quot;gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-development">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-growth-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%educated">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%brain-drain">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hi-dense-level">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Creative-dens">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-pop">
      <value value="1800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percapita">
      <value value="119000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Turtle_color">
      <value value="&quot;Creative&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segregation">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Display">
      <value value="&quot;Landuse&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="top10%">
      <value value="500000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Dev OFF Mov ON" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="240"/>
    <metric>count turtles with [creative-h = 1] / count turtles * 100</metric>
    <metric>count turtles with [creative = 1] / count turtles * 100</metric>
    <metric>indexofgini</metric>
    <metric>count patches with [creative-space &gt; 0] / 1681 * 100</metric>
    <metric>count turtles with [educated = 1] / count turtles * 100</metric>
    <metric>sum ([income] of turtles with [creative-h = 1])/(sum [income] of turtles) * 100</metric>
    <metric>mean [income] of turtles</metric>
    <metric>count turtles with [afford-rent = 1] / count turtles * 100</metric>
    <enumeratedValueSet variable="%PopHighCreative">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance-for-others">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-rent">
      <value value="72000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rent%-of-income">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-creative-space">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restrict-movement-to-neighborhood">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-dist">
      <value value="&quot;gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-development">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-growth-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%educated">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%brain-drain">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hi-dense-level">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Creative-dens">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-pop">
      <value value="1800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percapita">
      <value value="119000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Turtle_color">
      <value value="&quot;Creative&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segregation">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Display">
      <value value="&quot;Landuse&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="top10%">
      <value value="500000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Dev ON Mov OFF" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="240"/>
    <metric>count turtles with [creative-h = 1] / count turtles * 100</metric>
    <metric>count turtles with [creative = 1] / count turtles * 100</metric>
    <metric>indexofgini</metric>
    <metric>count patches with [creative-space &gt; 0] / 1681 * 100</metric>
    <metric>count turtles with [educated = 1] / count turtles * 100</metric>
    <metric>sum ([income] of turtles with [creative-h = 1])/(sum [income] of turtles) * 100</metric>
    <metric>mean [income] of turtles</metric>
    <metric>count turtles with [afford-rent = 1] / count turtles * 100</metric>
    <enumeratedValueSet variable="%PopHighCreative">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance-for-others">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-rent">
      <value value="72000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rent%-of-income">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-creative-space">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restrict-movement-to-neighborhood">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-dist">
      <value value="&quot;gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-development">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-growth-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%educated">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%brain-drain">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hi-dense-level">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Creative-dens">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-pop">
      <value value="1800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percapita">
      <value value="119000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Turtle_color">
      <value value="&quot;Creative&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segregation">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Display">
      <value value="&quot;Landuse&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="top10%">
      <value value="500000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="5 Year Default Seg" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="60"/>
    <metric>count turtles with [creative-h = 1] / count turtles * 100</metric>
    <metric>count turtles with [creative = 1] / count turtles * 100</metric>
    <metric>indexofgini</metric>
    <metric>count patches with [creative-space &gt; 0] / 1681 * 100</metric>
    <metric>sum ([income] of turtles with [creative-h = 1])/(sum [income] of turtles) * 100</metric>
    <metric>mean [income] of turtles</metric>
    <metric>count turtles with [afford-rent = 1] / count turtles * 100</metric>
    <enumeratedValueSet variable="%PopHighCreative">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance-for-others">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-rent">
      <value value="72000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rent%-of-income">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-creative-space">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restrict-movement-to-neighborhood">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-dist">
      <value value="&quot;gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-development">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-growth-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%educated">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%brain-drain">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hi-dense-level">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Creative-dens">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-pop">
      <value value="1800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percapita">
      <value value="120000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Turtle_color">
      <value value="&quot;Creative&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segregation">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Display">
      <value value="&quot;Landuse&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="top10%">
      <value value="350000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="1 Year Default Seg" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="12"/>
    <metric>count turtles with [creative-h = 1] / count turtles * 100</metric>
    <metric>count turtles with [creative = 1] / count turtles * 100</metric>
    <metric>indexofgini</metric>
    <metric>count patches with [creative-space &gt; 0] / 1681 * 100</metric>
    <metric>sum ([income] of turtles with [creative-h = 1])/(sum [income] of turtles) * 100</metric>
    <metric>mean [income] of turtles</metric>
    <metric>count turtles with [afford-rent = 1] / count turtles * 100</metric>
    <enumeratedValueSet variable="%PopHighCreative">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance-for-others">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-rent">
      <value value="72000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rent%-of-income">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-creative-space">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restrict-movement-to-neighborhood">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-dist">
      <value value="&quot;gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-development">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-growth-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%educated">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%brain-drain">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hi-dense-level">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Creative-dens">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-pop">
      <value value="1800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percapita">
      <value value="120000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Turtle_color">
      <value value="&quot;Creative&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segregation">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Display">
      <value value="&quot;Landuse&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="top10%">
      <value value="350000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="10 Year Default Seg" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="120"/>
    <metric>count turtles with [creative-h = 1] / count turtles * 100</metric>
    <metric>count turtles with [creative = 1] / count turtles * 100</metric>
    <metric>indexofgini</metric>
    <metric>count patches with [creative-space &gt; 0] / 1681 * 100</metric>
    <metric>sum ([income] of turtles with [creative-h = 1])/(sum [income] of turtles) * 100</metric>
    <metric>mean [income] of turtles</metric>
    <metric>count turtles with [afford-rent = 1] / count turtles * 100</metric>
    <enumeratedValueSet variable="%PopHighCreative">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance-for-others">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-rent">
      <value value="72000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rent%-of-income">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-creative-space">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restrict-movement-to-neighborhood">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-dist">
      <value value="&quot;gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-development">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-growth-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%educated">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%brain-drain">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hi-dense-level">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Creative-dens">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-pop">
      <value value="1800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percapita">
      <value value="120000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Turtle_color">
      <value value="&quot;Creative&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segregation">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Display">
      <value value="&quot;Landuse&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="top10%">
      <value value="350000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="15 Year Default Seg" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="180"/>
    <metric>count turtles with [creative-h = 1] / count turtles * 100</metric>
    <metric>count turtles with [creative = 1] / count turtles * 100</metric>
    <metric>indexofgini</metric>
    <metric>count patches with [creative-space &gt; 0] / 1681 * 100</metric>
    <metric>sum ([income] of turtles with [creative-h = 1])/(sum [income] of turtles) * 100</metric>
    <metric>mean [income] of turtles</metric>
    <metric>count turtles with [afford-rent = 1] / count turtles * 100</metric>
    <enumeratedValueSet variable="%PopHighCreative">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance-for-others">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-rent">
      <value value="72000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rent%-of-income">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-creative-space">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restrict-movement-to-neighborhood">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-dist">
      <value value="&quot;gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-development">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-growth-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%educated">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%brain-drain">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hi-dense-level">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Creative-dens">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-pop">
      <value value="1800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percapita">
      <value value="120000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Turtle_color">
      <value value="&quot;Creative&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segregation">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Display">
      <value value="&quot;Landuse&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="top10%">
      <value value="350000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20 Year Default Seg" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="240"/>
    <metric>count turtles with [creative-h = 1] / count turtles * 100</metric>
    <metric>count turtles with [creative = 1] / count turtles * 100</metric>
    <metric>indexofgini</metric>
    <metric>count patches with [creative-space &gt; 0] / 1681 * 100</metric>
    <metric>sum ([income] of turtles with [creative-h = 1])/(sum [income] of turtles) * 100</metric>
    <metric>mean [income] of turtles</metric>
    <metric>count turtles with [afford-rent = 1] / count turtles * 100</metric>
    <enumeratedValueSet variable="%PopHighCreative">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance-for-others">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-rent">
      <value value="72000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rent%-of-income">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-creative-space">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restrict-movement-to-neighborhood">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-dist">
      <value value="&quot;gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-development">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-growth-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%educated">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%brain-drain">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hi-dense-level">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Creative-dens">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-pop">
      <value value="1800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percapita">
      <value value="120000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Turtle_color">
      <value value="&quot;Creative&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segregation">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Display">
      <value value="&quot;Landuse&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="top10%">
      <value value="350000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Seg OFF Mov OFF Dev ON" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="120"/>
    <metric>count turtles with [creative-h = 1] / count turtles * 100</metric>
    <metric>count turtles with [creative = 1] / count turtles * 100</metric>
    <metric>indexofgini</metric>
    <metric>count patches with [creative-space &gt; 0] / 1681 * 100</metric>
    <metric>sum ([income] of turtles with [creative-h = 1])/(sum [income] of turtles) * 100</metric>
    <metric>mean [income] of turtles</metric>
    <metric>count turtles with [afford-rent = 1] / count turtles * 100</metric>
    <enumeratedValueSet variable="%PopHighCreative">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance-for-others">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-rent">
      <value value="72000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rent%-of-income">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-creative-space">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restrict-movement-to-neighborhood">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-dist">
      <value value="&quot;gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-development">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-growth-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%educated">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%brain-drain">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hi-dense-level">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Creative-dens">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-pop">
      <value value="1800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percapita">
      <value value="120000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Turtle_color">
      <value value="&quot;Creative&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segregation">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Display">
      <value value="&quot;Landuse&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="top10%">
      <value value="350000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Seg OFF Mov ON Dev OFF" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="120"/>
    <metric>count turtles with [creative-h = 1] / count turtles * 100</metric>
    <metric>count turtles with [creative = 1] / count turtles * 100</metric>
    <metric>indexofgini</metric>
    <metric>count patches with [creative-space &gt; 0] / 1681 * 100</metric>
    <metric>sum ([income] of turtles with [creative-h = 1])/(sum [income] of turtles) * 100</metric>
    <metric>mean [income] of turtles</metric>
    <metric>count turtles with [afford-rent = 1] / count turtles * 100</metric>
    <enumeratedValueSet variable="%PopHighCreative">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance-for-others">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-rent">
      <value value="72000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rent%-of-income">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-creative-space">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restrict-movement-to-neighborhood">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-dist">
      <value value="&quot;gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-development">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-growth-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%educated">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%brain-drain">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hi-dense-level">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Creative-dens">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-pop">
      <value value="1800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percapita">
      <value value="120000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Turtle_color">
      <value value="&quot;Creative&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segregation">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Display">
      <value value="&quot;Landuse&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="top10%">
      <value value="350000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Seg ON Mov OFF Dev OFF" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="120"/>
    <metric>count turtles with [creative-h = 1] / count turtles * 100</metric>
    <metric>count turtles with [creative = 1] / count turtles * 100</metric>
    <metric>indexofgini</metric>
    <metric>count patches with [creative-space &gt; 0] / 1681 * 100</metric>
    <metric>sum ([income] of turtles with [creative-h = 1])/(sum [income] of turtles) * 100</metric>
    <metric>mean [income] of turtles</metric>
    <metric>count turtles with [afford-rent = 1] / count turtles * 100</metric>
    <enumeratedValueSet variable="%PopHighCreative">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance-for-others">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-rent">
      <value value="72000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rent%-of-income">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-creative-space">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restrict-movement-to-neighborhood">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-dist">
      <value value="&quot;gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-development">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-growth-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%educated">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%brain-drain">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hi-dense-level">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Creative-dens">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-pop">
      <value value="1800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percapita">
      <value value="120000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Turtle_color">
      <value value="&quot;Creative&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segregation">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Display">
      <value value="&quot;Landuse&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="top10%">
      <value value="350000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Seg OFF Mov OFF Dev OFF" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="120"/>
    <metric>count turtles with [creative-h = 1] / count turtles * 100</metric>
    <metric>count turtles with [creative = 1] / count turtles * 100</metric>
    <metric>indexofgini</metric>
    <metric>count patches with [creative-space &gt; 0] / 1681 * 100</metric>
    <metric>sum ([income] of turtles with [creative-h = 1])/(sum [income] of turtles) * 100</metric>
    <metric>mean [income] of turtles</metric>
    <metric>count turtles with [afford-rent = 1] / count turtles * 100</metric>
    <enumeratedValueSet variable="%PopHighCreative">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance-for-others">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-rent">
      <value value="72000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rent%-of-income">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-creative-space">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restrict-movement-to-neighborhood">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-dist">
      <value value="&quot;gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allow-development">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-growth-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%educated">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%brain-drain">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hi-dense-level">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Creative-dens">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-pop">
      <value value="1800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percapita">
      <value value="120000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Turtle_color">
      <value value="&quot;Creative&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segregation">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="_Display">
      <value value="&quot;Landuse&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="top10%">
      <value value="350000"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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
