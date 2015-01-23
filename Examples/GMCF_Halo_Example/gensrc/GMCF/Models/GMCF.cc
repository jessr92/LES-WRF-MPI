// Generated wrapper for GMCF. Only parameter is $nmodels
#include "GMCF.h"
#include "CastPointers.h"
#include "GMCFmodelF.h"

 
int64_t GMCF::run_model12(SBA::System* sba_sysptr, SBA::Tile* sba_tileptr, uint64_t model_id) {
#ifdef VERBOSE
	std::cout << "INSIDE run_model12" << std::endl;
#endif
	const int model = 12;

    // Cast void* to int64_t
    // We need to cast to int64_t and then pass the address rather than casting to uint64_t* I think
#ifdef VERBOSE
	std::cout << "CHECKING pointers for MODEL 12: .tdc file name:" << sba_sysptr->task_description << "; Tile address: "<< sba_tileptr->address << std::endl;
#endif

#ifdef VERBOSE
	std::cout << "\n CASTING System pointer in model 12\n";

#endif

	void* sys_vp = reinterpret_cast<void*>(sba_sysptr);
	int64_t sys_iv = (int64_t)sys_vp;
	int64_t* sba_sys_ivp = &sys_iv;
#ifdef VERBOSE
	std::cout << "\n CASTING Tile pointer\n";
#endif
	void* tile_vp = reinterpret_cast<void*>(sba_tileptr);
	int64_t tile_iv = (int64_t)tile_vp;
    int64_t* sba_tile_ivp = &tile_iv;
#ifdef VERBOSE
	std::cout << "CALLING Fortran program_haloExchange3DRealExample_" << std::endl;
#endif

    // Here we call the actual Fortran function
    program_haloExchange3DRealExample_(sba_sys_ivp,sba_tile_ivp,&model);

#ifdef VERBOSE
	std::cout << "LEAVING run_model12" << std::endl;
#endif

    return 12; // purely for GPRM compatibility!
}		
