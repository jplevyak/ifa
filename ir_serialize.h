#ifndef _ir_serialize_h_
#define _ir_serialize_h_

#include "ifadefs.h"

// Backend entry point
void deserialize_ir(const char *filename, FA *fa);

// Frontend entry point
void serialize_ir(const char *filename, IF1 *if1, FA *fa);

#endif
