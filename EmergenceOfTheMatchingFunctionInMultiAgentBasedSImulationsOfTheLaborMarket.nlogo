globals
  [ U                                ;; number of unemployed people
    V                                ;; number of vacant jobs
    L                                ;; labour force ( U + number of employed people)
    uLevel                           ;; unemployment level ( L - number of employed people)
    u                                ;; unemployment rate ( U / L )
    v                                ;; vacancy rate ( V / L )
    p                                ;; participation rate ( L / size of the adult civilian population in working age )
    S                                ;; minimum salary
    matching-quality-threshold       ;; least necessary similarity
    firing-quality                   ;; least productivity necessary
    unexpected-firing
    max-productivity-fluctuation
    unexpected-company-motivation    ;; exceptional motivation for a company to find an employee, increases the similarity value of a matching
    unexpected-worker-motivation     ;; exceptional motivatoin for a worker to find a job, increases the similarity value of a matching
    exceptional-matching             ;; threshold below which the matching is considered to hold
    matching-random-pair-number ]    ;; in each matching round, a limited of random paris is considered, somehow representing the frictions resulting  from incomplete knowledge of the agents

breed [companies company]
breed [people person]
breed [matchers matcher]

turtles-own
  [ skills                           ;; skills of a worker       / skills needed for a job   (vector of 5 booleans)
    location                         ;; location of a worker     / location for a job        (discrete value)
    salary                           ;; salary/ effective salary          (minimum salary S) resp.)
    state                            ;; employed/unemployed      / filled/vacant             ()
    productivity ]                   ;; productivity of a worker / minimum productivity accepted by a company

matchers-own
  [ unemployed-people                ;; people unemployed
    vacant-job-companies ]           ;; companies with a vacant job



;;
;; Setup Procedures
;;
to setup
  clear-all
  ;;....
  reset-ticks
end



;;
;; Companies Procedures
;;
to ask-for-employee
  ;;....
end

to fire
  ;;...
end

to hire
  ;;...
end



;;
;; People Procedures
;;
to ask-for-job
  ;;...
end



;;
;; Matcher Procedures
;;
to match
  ;;...
end

to compute-similarity
  ;;...
end

to give-job
  ;;...
end

to give-employee
  ;;...
end

