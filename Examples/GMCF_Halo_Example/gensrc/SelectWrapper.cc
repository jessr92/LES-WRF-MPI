
#include "Services.h"

void Services::select_wrapper(unsigned int code) {

	switch (code) {
		case 288:
			kernel_GMCF();
			break;
		case 513:
			ls_BEGIN();
			break;
		default:
			none();
	};
}
