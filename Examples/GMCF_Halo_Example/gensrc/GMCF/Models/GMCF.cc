// Generated wrapper for GMCF. Only parameter is $nmodels
#include "GMCF.h"
#include "CastPointers.h"
#include "GMCFmodelF.h"

 
int64_t GMCF::run_model6(SBA::System* sba_sysptr, SBA::Tile* sba_tileptr, uint64_t model_id) {
#ifdef VERBOSE
	std::cout << "INSIDE run_model6" << std::endl;
#endif
	const int model = 6;

    // Cast void* to int64_t
    // We need to cast to int64_t and then pass the address rather than casting to uint64_t* I think
#ifdef VERBOSE
	std::cout << "CHECKING pointers for MODEL 6: .tdc file name:" << sba_sysptr->task_description << "; Tile address: "<< sba_tileptr->address << std::endl;
#endif

#ifdef VERBOSE
	std::cout << "\n CASTING System pointer in model 6\n";

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
	std::cout << "CALLING Fortran program_model6_" << std::endl;
#endif

    // Here we call the actual Fortran function
    program_model6_(sba_sys_ivp,sba_tile_ivp,&model);

#ifdef VERBOSE
	std::cout << "LEAVING run_model6" << std::endl;
#endif

    return 6; // purely for GPRM compatibility!
}		
 
int64_t GMCF::run_model11(SBA::System* sba_sysptr, SBA::Tile* sba_tileptr, uint64_t model_id) {
#ifdef VERBOSE
	std::cout << "INSIDE run_model11" << std::endl;
#endif
	const int model = 11;

    // Cast void* to int64_t
    // We need to cast to int64_t and then pass the address rather than casting to uint64_t* I think
#ifdef VERBOSE
	std::cout << "CHECKING pointers for MODEL 11: .tdc file name:" << sba_sysptr->task_description << "; Tile address: "<< sba_tileptr->address << std::endl;
#endif

#ifdef VERBOSE
	std::cout << "\n CASTING System pointer in model 11\n";

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
	std::cout << "CALLING Fortran program_model11_" << std::endl;
#endif

    // Here we call the actual Fortran function
    program_model11_(sba_sys_ivp,sba_tile_ivp,&model);

#ifdef VERBOSE
	std::cout << "LEAVING run_model11" << std::endl;
#endif

    return 11; // purely for GPRM compatibility!
}		
 
int64_t GMCF::run_model3(SBA::System* sba_sysptr, SBA::Tile* sba_tileptr, uint64_t model_id) {
#ifdef VERBOSE
	std::cout << "INSIDE run_model3" << std::endl;
#endif
	const int model = 3;

    // Cast void* to int64_t
    // We need to cast to int64_t and then pass the address rather than casting to uint64_t* I think
#ifdef VERBOSE
	std::cout << "CHECKING pointers for MODEL 3: .tdc file name:" << sba_sysptr->task_description << "; Tile address: "<< sba_tileptr->address << std::endl;
#endif

#ifdef VERBOSE
	std::cout << "\n CASTING System pointer in model 3\n";

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
	std::cout << "CALLING Fortran program_model3_" << std::endl;
#endif

    // Here we call the actual Fortran function
    program_model3_(sba_sys_ivp,sba_tile_ivp,&model);

#ifdef VERBOSE
	std::cout << "LEAVING run_model3" << std::endl;
#endif

    return 3; // purely for GPRM compatibility!
}		
 
int64_t GMCF::run_model7(SBA::System* sba_sysptr, SBA::Tile* sba_tileptr, uint64_t model_id) {
#ifdef VERBOSE
	std::cout << "INSIDE run_model7" << std::endl;
#endif
	const int model = 7;

    // Cast void* to int64_t
    // We need to cast to int64_t and then pass the address rather than casting to uint64_t* I think
#ifdef VERBOSE
	std::cout << "CHECKING pointers for MODEL 7: .tdc file name:" << sba_sysptr->task_description << "; Tile address: "<< sba_tileptr->address << std::endl;
#endif

#ifdef VERBOSE
	std::cout << "\n CASTING System pointer in model 7\n";

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
	std::cout << "CALLING Fortran program_model7_" << std::endl;
#endif

    // Here we call the actual Fortran function
    program_model7_(sba_sys_ivp,sba_tile_ivp,&model);

#ifdef VERBOSE
	std::cout << "LEAVING run_model7" << std::endl;
#endif

    return 7; // purely for GPRM compatibility!
}		
 
int64_t GMCF::run_model9(SBA::System* sba_sysptr, SBA::Tile* sba_tileptr, uint64_t model_id) {
#ifdef VERBOSE
	std::cout << "INSIDE run_model9" << std::endl;
#endif
	const int model = 9;

    // Cast void* to int64_t
    // We need to cast to int64_t and then pass the address rather than casting to uint64_t* I think
#ifdef VERBOSE
	std::cout << "CHECKING pointers for MODEL 9: .tdc file name:" << sba_sysptr->task_description << "; Tile address: "<< sba_tileptr->address << std::endl;
#endif

#ifdef VERBOSE
	std::cout << "\n CASTING System pointer in model 9\n";

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
	std::cout << "CALLING Fortran program_model9_" << std::endl;
#endif

    // Here we call the actual Fortran function
    program_model9_(sba_sys_ivp,sba_tile_ivp,&model);

#ifdef VERBOSE
	std::cout << "LEAVING run_model9" << std::endl;
#endif

    return 9; // purely for GPRM compatibility!
}		
 
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
	std::cout << "CALLING Fortran program_model12_" << std::endl;
#endif

    // Here we call the actual Fortran function
    program_model12_(sba_sys_ivp,sba_tile_ivp,&model);

#ifdef VERBOSE
	std::cout << "LEAVING run_model12" << std::endl;
#endif

    return 12; // purely for GPRM compatibility!
}		
 
int64_t GMCF::run_model2(SBA::System* sba_sysptr, SBA::Tile* sba_tileptr, uint64_t model_id) {
#ifdef VERBOSE
	std::cout << "INSIDE run_model2" << std::endl;
#endif
	const int model = 2;

    // Cast void* to int64_t
    // We need to cast to int64_t and then pass the address rather than casting to uint64_t* I think
#ifdef VERBOSE
	std::cout << "CHECKING pointers for MODEL 2: .tdc file name:" << sba_sysptr->task_description << "; Tile address: "<< sba_tileptr->address << std::endl;
#endif

#ifdef VERBOSE
	std::cout << "\n CASTING System pointer in model 2\n";

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
	std::cout << "CALLING Fortran program_model2_" << std::endl;
#endif

    // Here we call the actual Fortran function
    program_model2_(sba_sys_ivp,sba_tile_ivp,&model);

#ifdef VERBOSE
	std::cout << "LEAVING run_model2" << std::endl;
#endif

    return 2; // purely for GPRM compatibility!
}		
 
int64_t GMCF::run_model8(SBA::System* sba_sysptr, SBA::Tile* sba_tileptr, uint64_t model_id) {
#ifdef VERBOSE
	std::cout << "INSIDE run_model8" << std::endl;
#endif
	const int model = 8;

    // Cast void* to int64_t
    // We need to cast to int64_t and then pass the address rather than casting to uint64_t* I think
#ifdef VERBOSE
	std::cout << "CHECKING pointers for MODEL 8: .tdc file name:" << sba_sysptr->task_description << "; Tile address: "<< sba_tileptr->address << std::endl;
#endif

#ifdef VERBOSE
	std::cout << "\n CASTING System pointer in model 8\n";

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
	std::cout << "CALLING Fortran program_model8_" << std::endl;
#endif

    // Here we call the actual Fortran function
    program_model8_(sba_sys_ivp,sba_tile_ivp,&model);

#ifdef VERBOSE
	std::cout << "LEAVING run_model8" << std::endl;
#endif

    return 8; // purely for GPRM compatibility!
}		
 
int64_t GMCF::run_model1(SBA::System* sba_sysptr, SBA::Tile* sba_tileptr, uint64_t model_id) {
#ifdef VERBOSE
	std::cout << "INSIDE run_model1" << std::endl;
#endif
	const int model = 1;

    // Cast void* to int64_t
    // We need to cast to int64_t and then pass the address rather than casting to uint64_t* I think
#ifdef VERBOSE
	std::cout << "CHECKING pointers for MODEL 1: .tdc file name:" << sba_sysptr->task_description << "; Tile address: "<< sba_tileptr->address << std::endl;
#endif

#ifdef VERBOSE
	std::cout << "\n CASTING System pointer in model 1\n";

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
	std::cout << "CALLING Fortran program_model1_" << std::endl;
#endif

    // Here we call the actual Fortran function
    program_model1_(sba_sys_ivp,sba_tile_ivp,&model);

#ifdef VERBOSE
	std::cout << "LEAVING run_model1" << std::endl;
#endif

    return 1; // purely for GPRM compatibility!
}		
 
int64_t GMCF::run_model4(SBA::System* sba_sysptr, SBA::Tile* sba_tileptr, uint64_t model_id) {
#ifdef VERBOSE
	std::cout << "INSIDE run_model4" << std::endl;
#endif
	const int model = 4;

    // Cast void* to int64_t
    // We need to cast to int64_t and then pass the address rather than casting to uint64_t* I think
#ifdef VERBOSE
	std::cout << "CHECKING pointers for MODEL 4: .tdc file name:" << sba_sysptr->task_description << "; Tile address: "<< sba_tileptr->address << std::endl;
#endif

#ifdef VERBOSE
	std::cout << "\n CASTING System pointer in model 4\n";

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
	std::cout << "CALLING Fortran program_model4_" << std::endl;
#endif

    // Here we call the actual Fortran function
    program_model4_(sba_sys_ivp,sba_tile_ivp,&model);

#ifdef VERBOSE
	std::cout << "LEAVING run_model4" << std::endl;
#endif

    return 4; // purely for GPRM compatibility!
}		
 
int64_t GMCF::run_model10(SBA::System* sba_sysptr, SBA::Tile* sba_tileptr, uint64_t model_id) {
#ifdef VERBOSE
	std::cout << "INSIDE run_model10" << std::endl;
#endif
	const int model = 10;

    // Cast void* to int64_t
    // We need to cast to int64_t and then pass the address rather than casting to uint64_t* I think
#ifdef VERBOSE
	std::cout << "CHECKING pointers for MODEL 10: .tdc file name:" << sba_sysptr->task_description << "; Tile address: "<< sba_tileptr->address << std::endl;
#endif

#ifdef VERBOSE
	std::cout << "\n CASTING System pointer in model 10\n";

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
	std::cout << "CALLING Fortran program_model10_" << std::endl;
#endif

    // Here we call the actual Fortran function
    program_model10_(sba_sys_ivp,sba_tile_ivp,&model);

#ifdef VERBOSE
	std::cout << "LEAVING run_model10" << std::endl;
#endif

    return 10; // purely for GPRM compatibility!
}		
 
int64_t GMCF::run_model5(SBA::System* sba_sysptr, SBA::Tile* sba_tileptr, uint64_t model_id) {
#ifdef VERBOSE
	std::cout << "INSIDE run_model5" << std::endl;
#endif
	const int model = 5;

    // Cast void* to int64_t
    // We need to cast to int64_t and then pass the address rather than casting to uint64_t* I think
#ifdef VERBOSE
	std::cout << "CHECKING pointers for MODEL 5: .tdc file name:" << sba_sysptr->task_description << "; Tile address: "<< sba_tileptr->address << std::endl;
#endif

#ifdef VERBOSE
	std::cout << "\n CASTING System pointer in model 5\n";

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
	std::cout << "CALLING Fortran program_model5_" << std::endl;
#endif

    // Here we call the actual Fortran function
    program_model5_(sba_sys_ivp,sba_tile_ivp,&model);

#ifdef VERBOSE
	std::cout << "LEAVING run_model5" << std::endl;
#endif

    return 5; // purely for GPRM compatibility!
}		
