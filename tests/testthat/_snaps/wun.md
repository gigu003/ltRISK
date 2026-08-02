# wun validates surgery correction inputs

    Code
      do.call(wun, args)
    Condition
      Error:
      ! H and Ch must be provided when correct_for_surgery is TRUE

---

    Code
      do.call(wun, c(args, list(H = c(1, 1), Ch = c(2, 0))))
    Condition
      Error:
      ! Ch must not exceed H

# wun rejects an unidentified open interval

    Code
      wun(ages = 0, cancer = 1, cancer_death = 0, death = 0, pys = 1000)
    Condition
      Error in `wun()`:
      ! The open-ended age interval requires a positive all-cause mortality rate

