#include <stdbool.h>
#include <stdint.h>



void smp();

extern struct {  /* state */
  struct {  /* smp */
    uint64_t __channel_s2r;
    bool __channel_s2r_ready;
    struct {  /* source */
      bool done;
    } source;
    struct {  /* recv */
      bool done;
      uint64_t vote;
    } recv;
  } smp;
} state;



