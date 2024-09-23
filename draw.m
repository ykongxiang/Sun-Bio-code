% 读取数据
BMD_SN = readtable('H:\Sun Biology analysis\骨密度数据（原始）\weekly_BMD_SN.xlsx');

% 定义时间变量
t = 1:height(BMD_SN); % 假设每行代表一个时间点

% 绘图
figure;

% 创建子图
subplot(2,1,1);
plot(t, BMD_SN.mean_sunspot_detrend, 'b-', 'LineWidth', 1.5);
title('Detrended Mean Sunspot', 'FontSize', 18);
ylabel('Detrended Sunspot', 'FontSize', 11);
xticks(1:4:height(BMD_SN));
xlim([1, height(BMD_SN)]);
set(gca, 'FontSize', 14, 'LineWidth', 0.8);
xlabel('Time Points', 'FontSize', 10, 'LineWidth', 1.2);

% 创建第二个子图
subplot(2,1,2);
plot(t, BMD_SN.mean_BMD, 'r-', 'LineWidth', 1.5);
title('Mean BMD', 'FontSize', 18);
ylabel('BMD', 'FontSize', 11);
xticks(1:4:height(BMD_SN));
xlim([1, height(BMD_SN)]);
set(gca, 'FontSize', 14, 'LineWidth', 0.8);
xlabel('Time Points', 'FontSize', 10, 'LineWidth', 1.2);

% 调整图形布局
sgtitle('Mean Sunspot and Mean BMD', 'FontSize', 20);

