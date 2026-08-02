# ztest rejects duplicate ranges and zero standard errors

    Code
      ztest(duplicate, valid)
    Condition
      Error in `ztest_compare_frames()`:
      ! Each input must contain one estimate per start/end age range.

---

    Code
      ztest(zero, zero)
    Condition
      Error in `ztest_compare_frames()`:
      ! Combined standard errors must be positive and finite.

