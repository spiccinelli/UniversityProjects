import ROOT
import ROOT.ROOT as rr

##############################################################################################

find_idx_code ='''
using namespace ROOT::VecOps;
std::vector<int> find_idx(const RVec<int> &charge, const RVec<double> &phi, const RVec<double> &eta)
{
    std::vector<int> idx {-1,-1,-1};
    
    if(charge[0]==charge[1] && charge[1]!=charge[2]){
        
        idx[0] = 2;
        
        double dr_1 = DeltaR(eta[1], eta[2], phi[1], phi[2]);
        double dr_2 = DeltaR(eta[0], eta[2], phi[0], phi[2]);
        
        if(dr_1 < dr_2){
            idx[1] = 1;
            idx[2] = 0;
        }else{
            idx[1] = 0;
            idx[2] = 1;     
        }
      }
      
    if(charge[0]!=charge[1] && charge[1]!=charge[2]){
          
        idx[0] = 1;
        
        double dr_1 = DeltaR(eta[1], eta[2], phi[1], phi[2]);
        double dr_2 = DeltaR(eta[0], eta[1], phi[0], phi[1]);
        
        if(dr_1 < dr_2){
            idx[1] = 2;
            idx[2] = 0;
        }else{
            idx[1] = 0;
            idx[2] = 2;     
        }
      }
    
    if(charge[0]!=charge[1] && charge[1]==charge[2]){
          
          idx[0] = 0;
          
        double dr_1 = DeltaR(eta[0], eta[2], phi[0], phi[2]);
        double dr_2 = DeltaR(eta[0], eta[1], phi[0], phi[1]);
        
        if(dr_1 < dr_2){
            idx[1] = 2;
            idx[2] = 1;
        }else{
            idx[1] = 1;
            idx[2] = 2;     
        }
      }
      
    return idx;
};
'''

ROOT.gInterpreter.Declare(find_idx_code)

##############################################################################################

compute_m_code = '''
auto compute_m(const float &p1, const float &p2, const float &phi1, const float &phi2, const float &eta1, const float &eta2)
{
    //const auto dphi = Helper::DeltaPhi(phi1, phi2);
    return std::sqrt(2.0 * p1 * p2 * (std::cosh(eta1-eta2) - std::cos(phi1-phi2)));
};
'''

ROOT.gInterpreter.Declare(compute_m_code)

##############################################################################################

InvMass2_code = '''
using namespace ROOT::VecOps;

auto InvMass2(const float &p1, const float &p2, const float &eta1, const float &eta2, const float &phi1, const float &phi2, const float &m1, const float &m2)
{
    RVec<float> p = {p1, p2};
    RVec<float> eta = {eta1, eta2};
    RVec<float> phi = {phi1, phi2};
    RVec<float> mass = {m1, m2};

    return InvariantMass(p, eta, phi, mass);
};
'''

ROOT.gInterpreter.Declare(InvMass2_code)

##############################################################################################

InvMass3_code = '''
using namespace ROOT::VecOps;

auto InvMass3(const RVec<float> &p, const RVec<float> &eta, const RVec<float> &phi, const RVec<float> &m)
{
    return InvariantMass(p, eta, phi, m);
};
'''

ROOT.gInterpreter.Declare(InvMass3_code)

##############################################################################################

dR_code = '''
using namespace ROOT::VecOps;

auto dR(const float &eta1, const float &eta2, const float &phi1, const float &phi2)
{
    return DeltaR(eta1, eta2, phi1, phi2);
};
'''

ROOT.gInterpreter.Declare(dR_code)

##############################################################################################

dPhi_code = '''
using namespace ROOT::VecOps;

auto dPhi(const float &phi1, const float &phi2)
{
    return DeltaPhi(phi1, phi2);
};
'''

ROOT.gInterpreter.Declare(dPhi_code)

##############################################################################################

merge_code = '''
using namespace ROOT::VecOps;

auto Merge(const RVec<float> &mu, const RVec<float> &el)
{ 
    RVec<float> merged {-1,-1,-1};
    merged[0] = mu[0];
    merged[1] = mu[1];
    merged[2] = el[0]; 
    return merged;
}
'''

ROOT.gInterpreter.Declare(merge_code)

##############################################################################################

cleanJ_code = '''
using namespace ROOT::VecOps;

auto cleanJ(const RVec<float> &etaJ, const RVec<float> &etalep, const RVec<float> &phiJ, const RVec<float> &philep)
{
    RVec<int> mask(etaJ.size(), -1);
    int drJL = 0;
    
    for(int i=0; i<etaJ.size(); i++){
        for(int j=0; j<etalep.size(); j++){
            if(DeltaR(etaJ[i], etalep[j], phiJ[i], philep[j]) > 0.4 && abs(etaJ[i]) < 2.5){
                drJL++;
            }
        }
        if(drJL == etalep.size()){
            mask[i] = 1;
        }
        else{
            mask[i] = 0;
        }
        drJL = 0;
    }
    
    return mask;
};
'''

ROOT.gInterpreter.Declare(cleanJ_code)

##############################################################################################

prod_code = '''
using namespace ROOT::VecOps;

auto prod(const RVec<int> &v1, const RVec<int> &v2)
{
    RVec<int> p(v1.size(), 0);
    
    for(int i=0; i<v1.size(); i++){
        p[i] = v1[i]*v2[i];
    }
    
    return p;
};
'''

ROOT.gInterpreter.Declare(prod_code)

##############################################################################################

closest_Jet_code = '''
using namespace ROOT::VecOps;

auto closest_Jet(const RVec<float> &etaJ, const float &etalep, const RVec<float> &phiJ, const float &philep)
{
    float dR_min = 1000;
    float dR;
    
    for(int i=0; i<etaJ.size(); i++){
            dR = DeltaR(etaJ[i], etalep, phiJ[i], philep);
            if (dR < dR_min){
                dR_min = dR;
            }
    }   
    return dR_min;
};
'''

ROOT.gInterpreter.Declare(closest_Jet_code)

##############################################################################################

higherELepton_code = '''
using namespace ROOT::VecOps;

auto higherELepton(const RVec<float> &pt, const RVec<float> &eta, const RVec<float> &phi, bool min=0)
{
    RVec<float> eta_phi(2, 0);
    float max_pt = 0.;
    float min_pt = 1e4;
    
    if (!min){
        for(int i=0; i<pt.size(); i++){
                if (pt[i] > max_pt){
                    max_pt = pt[i];
                    eta_phi[0] = eta[i];
                    eta_phi[1] = phi[i];
                }
        }
    }
    
    else{
        for(int i=0; i<pt.size(); i++){
                if (pt[i] < min_pt){
                    min_pt = pt[i];
                    eta_phi[0] = eta[i];
                    eta_phi[1] = phi[i];
                }
        }
    }
    return eta_phi;
};
'''

ROOT.gInterpreter.Declare(higherELepton_code)

##############################################################################################

diJ_invm_code = '''
using namespace ROOT::VecOps;

RVec<float> diJ_invm(const RVec<float> &p, const RVec<float> &eta, const RVec<float> &phi, const RVec<float> &m)
{
    int dim = p.size()*(p.size()-1)/2;
    RVec<int> inv_m(dim, 0);
    int counter = 0;
    
    RVec<float> p_ = {0, 0};
    RVec<float> eta_ = {0, 0};
    RVec<float> phi_ = {0, 0};
    RVec<float> mass_ = {0, 0};
    
    for(int i=0; i<(p.size()-1); i++){
        for(int j=i+1; j<p.size(); j++){
            p_[0] = p[i];
            p_[1] = p[j];
            eta_[0] = eta[i];
            eta_[1] = eta[j];
            phi_[0] = phi[i];
            phi_[1] = phi[j];
            mass_[0] = m[i];
            mass_[1] = m[j];

            inv_m[counter] = InvariantMass(p_, eta_, phi_, mass_);
            counter++;
        }
    }
    return inv_m;
};
'''

ROOT.gInterpreter.Declare(diJ_invm_code)
