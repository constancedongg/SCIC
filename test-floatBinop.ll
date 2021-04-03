; ModuleID = 'scic'
source_filename = "scic"

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.1 = private unnamed_addr constant [4 x i8] c"%g\0A\00"
@fmt.2 = private unnamed_addr constant [4 x i8] c"%s\0A\00"

declare i8* @printf(i8*, ...)

declare i32 @printbig(i32)

define i32 @main() {
entry:
  %printf = call i8* (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.1, i32 0, i32 0), double 4.076000e+00)
  %printf1 = call i8* (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.1, i32 0, i32 0), double -1.676000e+00)
  %printf2 = call i8* (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.1, i32 0, i32 0), double 0x400B9C0EBEDFA43F)
  %printf3 = call i8* (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.1, i32 0, i32 0), double 5.000000e-01)
  %printf4 = call i8* (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.1, i32 0, i32 0), double 0x3FDAB8EB281560BC)
  %printf5 = call i8* (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i1 true)
  %printf6 = call i8* (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i1 true)
  %printf7 = call i8* (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i1 true)
  %printf8 = call i8* (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i1 false)
  %pow32 = call double @llvm.pow.f64(double 2.000000e+00, double 3.000000e+00)
  %printf9 = call i8* (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.1, i32 0, i32 0), double %pow32)
  %pow3210 = call double @llvm.pow.f64(double 2.000000e+00, double 1.500000e+00)
  %printf11 = call i8* (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.1, i32 0, i32 0), double %pow3210)
  %powi32 = call double @llvm.powi.f64(double 3.000000e+00, i32 2)
  %printf12 = call i8* (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.1, i32 0, i32 0), double %powi32)
  %powi3213 = call double @llvm.powi.f64(double 5.500000e+00, i32 3)
  %printf14 = call i8* (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.1, i32 0, i32 0), double %powi3213)
  ret i32 0
}

; Function Attrs: nounwind readnone speculatable
declare double @llvm.pow.f64(double, double) #0

; Function Attrs: nounwind readnone speculatable
declare double @llvm.powi.f64(double, i32) #0

attributes #0 = { nounwind readnone speculatable }
