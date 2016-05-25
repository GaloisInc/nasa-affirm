#include <stdbool.h>
#include <stdint.h>



void om1();

extern struct {  /* state */
  struct {  /* om1 */
    uint64_t c1v;
    bool hasData;
    struct {  /* source */
      bool done;
      uint64_t msg;
    } source;
  } om1;
} state;



