clearvars -except current_timestep next_timestep gridlabd GLD_OK ans DR_signal

%format long g

current_timestep = gridlabd.global.clock;

if current_timestep >= next_timestep; %we have advanced in time
        next_timestep = current_timestep+1; %update next_timestep
else %we are in the same timestep
        next_timestep = next_timestep; %our next_timestep is the same
end

m=1;
csvwrite('wait.txt',m);      %wait file for matlab, will be deleted by FLG
csvwrite('ext_signal.txt',DR_signal);   %writes file with demand respond signal for TCL
csvwrite('iamtoo.txt',m);    %FLG trigger file
while (exist('wait.txt', 'file'))
   pause(0.001); %pauses 1 milisecond if file exists, until its deleted
end
meter_load = csvread('load_output.csv');
load_1 = meter_load(1,1);
load_2 = meter_load(2,1);
load_3 = meter_load(3,1);
load_4 = meter_load(4,1);
load_5 = meter_load(5,1);
load_6 = meter_load(6,1);
load_7 = meter_load(7,1);
load_8 = meter_load(8,1);
load_9 = meter_load(9,1);
load_10 = meter_load(10,1);

ans=next_timestep;
