# ir_a1

1. <b>Parallelism</b>. Reduce function has been used everywhere because all collection have parallelism, e.g. they have no order of cumulation (during finding distinct tokens and summing). And ParSet ++ Set used in order to do parallel merging
2. <b>Cash Reuse</b>. DocVector is used in order to reuse vocab cash with quick look up by doc-> word -> word frequency 
3. <b>Preprocessing</b> all stop words have been filtered
