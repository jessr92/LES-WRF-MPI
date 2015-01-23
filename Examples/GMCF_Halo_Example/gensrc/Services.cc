#include "Services.h"
#include "GMCF.h" 
#include "SystemConfiguration.h"

using namespace SBA;

void Services::kernel_GMCF() {
#ifdef VERBOSE
    std::cout << "CALLING kernel_GMCF()" << std::endl ; 
#endif    
    GMCF* inst;
    if (init_state(SC_GMCF_GMCF)) {
        inst = new GMCF();
        store_state(SC_GMCF_GMCF,(void*)inst);
    } else {
        inst=(GMCF*)load_state(SC_GMCF_GMCF);
    }

    void* res;
	Symbol_t res_symbol = NIHIL;
#ifdef VERBOSE
    std::cout << "CALLING method "<< method() << std::endl; 
#endif     
    switch ( method() ) {
        case M_GMCF_GMCF_run_model12:
		{
			int64_t retval = inst->run_model12((SBA::System*)sba_system_ptr, (SBA::Tile*)sba_tile_ptr, (uint64_t)arg(0));
			res = (void*)retval;
			res_symbol=mkPointerSymbol(res);
			break;
		};

		default:
			std::cout << "ERROR: NO SUCH METHOD: " << method() << "for class GMCF\n";
    };
    result(res_symbol);
}	
