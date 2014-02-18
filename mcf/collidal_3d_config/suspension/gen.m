# generate parameters for the simulations


arg_list = argv ();
if length(arg_list)>0
  n= eval(arg_list{1})
else
  n = 1; 
endif

ycenter = 4.0;
a       = 1.0;
# add some noise
fuzzy = 1e-3
colr = linspace(0.5, 0.95, 5) + fuzzy;
colz = ycenter + a + 1.1*colr;

filename = sprintf("vars.mcf.%i", n);
disp(filename);
fid = fopen (filename, "w");

fprintf(fid, "COLR=%9.3e\n", colr(n));
fprintf(fid, "COLZ=%9.3e\n", colz(n));

fclose(fid);
