---
author:
- Josh Cowley
authors:
- Josh Cowley
date: 04 November, 2022
title: JAGS Sampling Controls
toc-title: Table of contents
---

## Debug (1k)

Used for debugging any JAGS related code.

::: cell
``` {.r .cell-code}
jags_n_debug <-
  mcmcrutils::jags_n(
    n.adapt = 1e2,
    n.update = 1e2,
    n.iter = 2e3,
    n.thin = 2,
    n.chains = 2
  )

usethis::use_data(jags_n_debug, overwrite = TRUE)
```
:::

## Short Analysis (10k)

Analysis of data, but fewer samples.

::: cell
``` {.r .cell-code}
jags_n_short <-
  mcmcrutils::jags_n(
    n.adapt = 1e4,
    n.update = 1e4,
    n.iter = 1e4,
    n.thin = 1,
    n.chains = 2
  )

usethis::use_data(jags_n_short, overwrite = TRUE)
```
:::

## Thin (50k / 5)

Longer analysis of data *with* thinning for less storage overhead.

::: cell
``` {.r .cell-code}
jags_n_thin <-
  mcmcrutils::jags_n(
    n.adapt = 1e4,
    n.update = 1e4,
    n.iter = 5e4,
    n.thin = 5,
    n.chains = 2
  )

usethis::use_data(jags_n_thin, overwrite = TRUE)
```
:::
