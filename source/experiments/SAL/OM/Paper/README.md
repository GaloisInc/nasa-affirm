SAL files associated with the paper.

* om1.sal: Full model. (Complete)
* om1_arch.sal: Model with alternate architecture. (TODO)
* om1_just_byz.sal: Model with just Byzantine Faults. (Complete)
* om1_mvs.sal: Model with mid-value selection instead of fast_mjry (TODO)
* om1_synch.sal: Synchronous model (TODO)

These can be verified by running, e.g.:

```
> ./runproof.sh om1.sal -par "{3,2}"
```

to run the proofs in `om1.sal` for 3 relays and 2 receivers.
