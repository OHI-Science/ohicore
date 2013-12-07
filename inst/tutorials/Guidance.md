Guidance on Naming Conventions
========================================================

Case sensitive.

## Overview
From a top-down perspective, the Index Score is a weighted average of the Goal Scores, derived by the goal's Dimensions. We generally refer to these all as having "score" units. The Dimensions are calculated based on goal Functions for Status and Trend, and weighted matrices for Pressures and Resilience, using input Layers.

Two types of datasets are available for loading from within the `ohicore` package (and perhaps as a seperate `ohidata` package in future): input Layers and output Scores, each having their own custom R class for specialized methods and properties, particularly related to composing and summarizing.

1. layers.[RegionYear].[VersionYear]
1. scores.[RegionYear].[VersionYear] 

## Layers

## Scores

### Goals
The convention for goal names is to use three letter codes for goals without subgoals, and two letter codes for those which have subgoals, ie supragoals. For ease of data subsetting, Index is listed amongst the goals, but refers to the weighted average of goal scores.

### Dimensions

These are all referred to as "scores". Here are the variations on the short | full name:

1. **status** | Status
1. **trend** | Trend  
1. **pressures** | Pressures
1. **resilience** | Resilience
1. **future** | Likely Future State
1. **score** | Score. When specific to a goal, this is a goal score. Whereas when applicable to Index, 
  



