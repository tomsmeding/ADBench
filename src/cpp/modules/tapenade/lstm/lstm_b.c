/*
 *   This file is a repaired version of the file "gmm_b_tapenade_generated.c"
 *   Fixing list (all fixings are marked by the comment TFIX):
 *
 * N    Wrong code                                                   Line   Fixed code 
 * --------------------------------------------------------------------------------------------------------------------------------------
 * 1)   #include <adBuffer.h>                                      =>(41)   #include "lstm_b.h"
 * --------------------------------------------------------------------------------------------------------------------------------------
 * 2)   pushReal8(s[i + b]);                                       =>(233)  pushReal8Array(s + i, 2 * b); 
 *      pushReal8(s[i]);
 * --------------------------------------------------------------------------------------------------------------------------------------
 * 3)   for (i = 2*l*b-(2*l*b-1)%(2*b)-1; i <= 0; i += -(2*b)) {   =>(254)  for (i = 2*l*b-(2*l*b-1)%(2*b)-1; i >= 0; i += -(2*b)) {
 * --------------------------------------------------------------------------------------------------------------------------------------
 * 4)   popReal8(&(s[i]));                                         =>(257)  popReal8Array(s + i, 2 * b);
 *      popReal8(&(s[i + b]));
 * --------------------------------------------------------------------------------------------------------------------------------------
 * 5)   double *stateb;                                            =>(306)  double* stateb = (double*)malloc(2 * l * b * sizeof(double));
 * --------------------------------------------------------------------------------------------------------------------------------------
 * 6)   const double *ygold;                                       =>(317)  const double* ygold = NULL;
 * --------------------------------------------------------------------------------------------------------------------------------------
 * 7)   pushReal8(*ypred);                                         =>(322)  pushReal8Array(ypred, b);
 * --------------------------------------------------------------------------------------------------------------------------------------
 * 8)   pushReal8(*state);                                         =>(326)  pushReal8Array(state, 2 * b * l);
 * --------------------------------------------------------------------------------------------------------------------------------------
 * 9)   *stateb = 0.0;                                             =>(339)  for (t = 0; t < 2 * l * b; t++)
 *                                                                              stateb[t] = 0.0;
 * --------------------------------------------------------------------------------------------------------------------------------------
 * 10)  for (t = (c-1)*b-((c-1)*b-1)%b-1; t <= 0; t += -b) {       =>(341)  for (t = (c-1)*b-((c-1)*b-1)%b-1; t >= 0; t += -b) {
 * --------------------------------------------------------------------------------------------------------------------------------------
 * 11)  popReal8(state);                                           =>(353)  popReal8Array(state, 2 * b * l);
 * --------------------------------------------------------------------------------------------------------------------------------------
 * 12)  popReal8(ypred);                                           =>(356)  popReal8Array(ypred, b);
 * --------------------------------------------------------------------------------------------------------------------------------------
 * 13)                                                             =>(364)  free(stateb);
 * --------------------------------------------------------------------------------------------------------------------------------------
 */

/*        Generated by TAPENADE     (INRIA, Ecuador team)
    Tapenade 3.14 (r7259) - 18 Jan 2019 09:35
*/
#include "lstm_b.h" /* TFIX */

/*
  Differentiation of sigmoid in reverse (adjoint) mode:
   gradient     of useful results: sigmoid
   with respect to varying inputs: x
*/
// UTILS
// Sigmoid on scalar
void sigmoid_b(double x, double *xb, double sigmoidb) {
    double temp;
    double sigmoid;
    temp = exp(-x) + 1.0;
    *xb = exp(-x)*sigmoidb/(temp*temp);
}

// UTILS
// Sigmoid on scalar
double sigmoid_nodiff(double x) {
    return 1.0/(1.0+exp(-x));
}

/*
  Differentiation of logsumexp in reverse (adjoint) mode:
   gradient     of useful results: logsumexp *vect
   with respect to varying inputs: *vect
   Plus diff mem management of: vect:in
*/
// log(sum(exp(x), 2))
void logsumexp_b(const double *vect, double *vectb, int sz, double logsumexpb)
{
    double sum = 0.0;
    double sumb = 0.0;
    int i;
    double logsumexp;
    for (i = 0; i < sz; ++i)
        sum = sum + exp(vect[i]);
    sum = sum + 2;
    sumb = logsumexpb/sum;
    for (i = sz-1; i > -1; --i)
        vectb[i] = vectb[i] + exp(vect[i])*sumb;
}

// log(sum(exp(x), 2))
double logsumexp_nodiff(const double *vect, int sz) {
    double sum = 0.0;
    int i;
    for (i = 0; i < sz; ++i)
        sum += exp(vect[i]);
    sum += 2;
    return log(sum);
}

/*
  Differentiation of lstm_model in reverse (adjoint) mode:
   gradient     of useful results: alloc(*gates) *cell *bias *hidden
                *weight *input
   with respect to varying inputs: alloc(*gates) *cell *bias *hidden
                *weight *input
   Plus diff mem management of: cell:in bias:in hidden:in weight:in
                input:in
*/
// LSTM OBJECTIVE
// The LSTM model
void lstm_model_b(int hsize, const double *weight, double *weightb, const 
        double *bias, double *biasb, double *hidden, double *hiddenb, double *
        cell, double *cellb, const double *input, double *inputb) {
    double *gates;
    double *gatesb;
    double arg1;
    double arg1b;
    int ii1;
    double temp;
    double tempb;
    gatesb = (double *)malloc(4*hsize*sizeof(double));
    for (ii1 = 0; ii1 < 4*hsize; ++ii1)
        gatesb[ii1] = 0.0;
    gates = (double *)malloc(4*hsize*sizeof(double));
    double *forget = &(gates[0]);
    double *forgetb = &(gatesb[0]);
    double *ingate = &(gates[hsize]);
    double *ingateb = &(gatesb[hsize]);
    double *outgate = &(gates[2*hsize]);
    double *outgateb = &(gatesb[2*hsize]);
    double *change = &(gates[3*hsize]);
    double *changeb = &(gatesb[3*hsize]);
    int i;
    for (i = 0; i < hsize; ++i) {
        arg1 = input[i]*weight[i] + bias[i];
        forget[i] = sigmoid_nodiff(arg1);
        arg1 = hidden[i]*weight[hsize+i] + bias[hsize + i];
        ingate[i] = sigmoid_nodiff(arg1);
        arg1 = input[i]*weight[2*hsize+i] + bias[2*hsize + i];
        outgate[i] = sigmoid_nodiff(arg1);
        change[i] = tanh(hidden[i]*weight[3*hsize+i] + bias[3*hsize + i]);
    }
    for (i = 0; i < hsize; ++i) {
        pushReal8(cell[i]);
        cell[i] = cell[i]*forget[i] + ingate[i]*change[i];
    }
    for (i = hsize-1; i > -1; --i) {
        outgateb[i] = outgateb[i] + tanh(cell[i])*hiddenb[i];
        cellb[i] = cellb[i] + outgate[i]*(1.0-tanh(cell[i])*tanh(cell[i]))*
            hiddenb[i];
        hiddenb[i] = 0.0;
    }
    for (i = hsize-1; i > -1; --i) {
        popReal8(&(cell[i]));
        forgetb[i] = forgetb[i] + cell[i]*cellb[i];
        ingateb[i] = ingateb[i] + change[i]*cellb[i];
        changeb[i] = changeb[i] + ingate[i]*cellb[i];
        cellb[i] = forget[i]*cellb[i];
    }
    for (i = hsize-1; i > -1; --i) {
        temp = weight[3*hsize + i];
        tempb = (1.0-tanh(hidden[i]*temp+bias[3*hsize+i])*tanh(hidden[i]*temp+
            bias[3*hsize+i]))*changeb[i];
        hiddenb[i] = hiddenb[i] + temp*tempb;
        weightb[3*hsize + i] = weightb[3*hsize + i] + hidden[i]*tempb;
        biasb[3*hsize + i] = biasb[3*hsize + i] + tempb;
        changeb[i] = 0.0;
        arg1 = input[i]*weight[2*hsize+i] + bias[2*hsize + i];
        sigmoid_b(arg1, &arg1b, outgateb[i]);
        outgateb[i] = 0.0;
        inputb[i] = inputb[i] + weight[2*hsize+i]*arg1b;
        weightb[2*hsize + i] = weightb[2*hsize + i] + input[i]*arg1b;
        biasb[2*hsize + i] = biasb[2*hsize + i] + arg1b;
        arg1 = hidden[i]*weight[hsize+i] + bias[hsize + i];
        sigmoid_b(arg1, &arg1b, ingateb[i]);
        ingateb[i] = 0.0;
        hiddenb[i] = hiddenb[i] + weight[hsize+i]*arg1b;
        weightb[hsize + i] = weightb[hsize + i] + hidden[i]*arg1b;
        biasb[hsize + i] = biasb[hsize + i] + arg1b;
        arg1 = input[i]*weight[i] + bias[i];
        sigmoid_b(arg1, &arg1b, forgetb[i]);
        forgetb[i] = 0.0;
        inputb[i] = inputb[i] + weight[i]*arg1b;
        weightb[i] = weightb[i] + input[i]*arg1b;
        biasb[i] = biasb[i] + arg1b;
    }
    free(gates);
    free(gatesb);
}

// LSTM OBJECTIVE
// The LSTM model
void lstm_model_nodiff(int hsize, const double *weight, const double *bias, 
        double *hidden, double *cell, const double *input) {
    double *gates;
    double arg1;
    gates = (double *)malloc(4*hsize*sizeof(double));
    double *forget = &(gates[0]);
    double *ingate = &(gates[hsize]);
    double *outgate = &(gates[2*hsize]);
    double *change = &(gates[3*hsize]);
    int i;
    for (i = 0; i < hsize; ++i) {
        arg1 = input[i]*weight[i] + bias[i];
        forget[i] = sigmoid_nodiff(arg1);
        arg1 = hidden[i]*weight[hsize+i] + bias[hsize + i];
        ingate[i] = sigmoid_nodiff(arg1);
        arg1 = input[i]*weight[2*hsize+i] + bias[2*hsize + i];
        outgate[i] = sigmoid_nodiff(arg1);
        change[i] = tanh(hidden[i]*weight[3*hsize+i] + bias[3*hsize + i]);
    }
    for (i = 0; i < hsize; ++i)
        cell[i] = cell[i]*forget[i] + ingate[i]*change[i];
    for (i = 0; i < hsize; ++i)
        hidden[i] = outgate[i]*tanh(cell[i]);
    free(gates);
}

/*
  Differentiation of lstm_predict in reverse (adjoint) mode:
   gradient     of useful results: alloc(*gates) *s *w *w2 *x2
   with respect to varying inputs: alloc(*gates) *s *w *w2 *x2
   Plus diff mem management of: s:in w:in w2:in x2:in
*/
// Predict LSTM output given an input
void lstm_predict_b(int l, int b, const double *w, double *wb, const double *
        w2, double *w2b, double *s, double *sb, const double *x, double *x2, 
        double *x2b) {
    int i;
    double tmp;
    double tmpb;
    for (i = 0; i < b; ++i) {
        pushReal8(x2[i]);
        x2[i] = x[i]*w2[i];
    }
    double *xp = x2;
    double *xpb = x2b;
    for (i = 0; i <= 2*l*b-1; i += 2*b) {
        pushReal8Array(s + i, 2 * b); /* TFIX */
        lstm_model_nodiff(b, &(w[i*4]), &(w[(i+b)*4]), &(s[i]), &(s[i + b]), 
                          xp);
        pushPointer8(xpb);
        xpb = &(sb[i]);
        pushPointer8(xp);
        xp = &(s[i]);
    }
    for (i = 0; i < b; ++i) {
        tmp = xp[i]*w2[b+i] + w2[2*b + i];
        pushReal8(x2[i]);
        x2[i] = tmp;
    }
    for (i = b-1; i > -1; --i) {
        popReal8(&(x2[i]));
        tmpb = x2b[i];
        x2b[i] = 0.0;
        xpb[i] = xpb[i] + w2[b+i]*tmpb;
        w2b[b + i] = w2b[b + i] + xp[i]*tmpb;
        w2b[2*b + i] = w2b[2*b + i] + tmpb;
    }
    for (i = 2*l*b-(2*l*b-1)%(2*b)-1; i >= 0; i += -(2*b)) { /* TFIX */
        popPointer8((void **)&xp);
        popPointer8((void **)&xpb);
        popReal8Array(s + i, 2 * b); /* TFIX */
        lstm_model_b(b, &(w[i*4]), &(wb[i*4]), &(w[(i+b)*4]), &(wb[(i+b)*4]), 
                     &(s[i]), &(sb[i]), &(s[i + b]), &(sb[i + b]), xp, xpb);
    }
    for (i = b-1; i > -1; --i) {
        popReal8(&(x2[i]));
        w2b[i] = w2b[i] + x[i]*x2b[i];
        x2b[i] = 0.0;
    }
}

// Predict LSTM output given an input
void lstm_predict_nodiff(int l, int b, const double *w, const double *w2, 
        double *s, const double *x, double *x2) {
    int i;
    for (i = 0; i < b; ++i)
        x2[i] = x[i]*w2[i];
    double *xp = x2;
    for (i = 0; i <= 2*l*b-1; i += 2*b) {
        lstm_model_nodiff(b, &(w[i*4]), &(w[(i+b)*4]), &(s[i]), &(s[i + b]), 
                          xp);
        xp = &(s[i]);
    }
    for (i = 0; i < b; ++i)
        x2[i] = xp[i]*w2[b+i] + w2[2*b + i];
}

/*
  Differentiation of lstm_objective in reverse (adjoint) mode:
   gradient     of useful results: *loss
   with respect to varying inputs: *main_params *extra_params
                *loss
   RW status of diff variables: *main_params:out *extra_params:out
                *loss:in-out
   Plus diff mem management of: extra_params:in loss:in
*/
// LSTM objective (loss function)
void lstm_objective_b(int l, int c, int b, const double *main_params, double *
        main_paramsb, const double *extra_params, double *extra_paramsb, 
        double *state, const double *sequence, double *loss, double *lossb) {
    int i, t;
    double total = 0.0;
    double totalb = 0.0;
    int count = 0;
    const double *input = &(sequence[0]);
    double *ypred;
    double *ypredb;
    int ii1;
    int branch;
    double* stateb = (double*)malloc(2 * l * b * sizeof(double)); /* TFIX */
    ypredb = (double *)malloc(b*sizeof(double));
    for (ii1 = 0; ii1 < b; ++ii1)
        ypredb[ii1] = 0.0;
    ypred = (double *)malloc(b*sizeof(double));
    double *ynorm;
    double *ynormb;
    ynormb = (double *)malloc(b*sizeof(double));
    for (ii1 = 0; ii1 < b; ++ii1)
        ynormb[ii1] = 0.0;
    ynorm = (double *)malloc(b*sizeof(double));
    const double* ygold = NULL; /* TFIX */
    double lse;
    double lseb;
    for (t = 0; t <= (c-1)*b-1; t += b) {
        if (ypred) {
            pushReal8Array(ypred, b); /* TFIX */
            pushControl1b(1);
        } else
            pushControl1b(0);
        pushReal8Array(state, 2 * b * l); /* TFIX */
        lstm_predict_nodiff(l, b, main_params, extra_params, state, input, 
                            ypred);
        pushPointer8(ygold);
        ygold = &(sequence[t + b]);
        count = count + b;
        pushPointer8(input);
        input = ygold;
    }
    totalb = -(*lossb/count);
    *lossb = 0.0;
    *main_paramsb = 0.0;
    *extra_paramsb = 0.0;
    for (t = 0; t < 2 * l * b; t++) /* TFIX */
        stateb[t] = 0.0;
    for (t = (c-1)*b-((c-1)*b-1)%b-1; t >= 0; t += -b) { /* TFIX */
        popPointer8((void **)&input);
        for (i = b-1; i > -1; --i)
            ynormb[i] = ynormb[i] + ygold[i]*totalb;
        popPointer8((void **)&ygold);
        lseb = 0.0;
        for (i = b-1; i > -1; --i) {
            ypredb[i] = ypredb[i] + ynormb[i];
            lseb = lseb - ynormb[i];
            ynormb[i] = 0.0;
        }
        logsumexp_b(ypred, ypredb, b, lseb);
        popReal8Array(state, 2 * b * l); /* TFIX */
        popControl1b(&branch);
        if (branch == 1)
            popReal8Array(ypred, b); /* TFIX */
        lstm_predict_b(l, b, main_params, main_paramsb, extra_params, 
                       extra_paramsb, state, stateb, input, ypred, ypredb);
    }
    free(ynorm);
    free(ynormb);
    free(ypred);
    free(ypredb);
    free(stateb); /* TFIX */ // Added to dispose memory allocated in repaired code
}
