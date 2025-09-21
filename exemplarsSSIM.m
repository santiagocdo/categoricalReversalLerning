% Clear workspace and command window
clear; clc;

% Define the folder containing the PNG images
folder_path = 'images/';

% Get a list of all PNG files in the folder
file_list = dir(fullfile(folder_path, '*.jpg'));

% Get the number of images
num_images = numel(file_list);

% Pre-allocate a matrix to store the SSIM values
% The matrix will be a 16x16 grid for all-to-all comparisons
ssim_matrix = zeros(num_images, num_images);

% Loop through each image to compare it with all other images
for i = 1:num_images
    % Read the first image
    image1_path = fullfile(folder_path, file_list(i).name);
    image1 = imread(image1_path);
    
    % Ensure images are grayscale for SSIM calculation
    if size(image1, 3) == 3
        image1 = rgb2gray(image1);
    end
    
    for j = 1:num_images
        % Read the second image
        image2_path = fullfile(folder_path, file_list(j).name);
        image2 = imread(image2_path);
        
        % Ensure images are grayscale for SSIM calculation
        if size(image2, 3) == 3
            image2 = rgb2gray(image2);
        end
        
        % Calculate the SSIM value
        [ssim_val, ~] = ssim(image1, image2);
        
        % Store the SSIM value in the matrix
        ssim_matrix(i, j) = ssim_val;
    end
end

% Display the resulting SSIM matrix
disp('SSIM Matrix (All-to-All Comparison):');
disp(ssim_matrix);

% Optional: Save the SSIM matrix to a CSV file for easy viewing
csv_filename = 'ssim_results.csv';
writematrix(ssim_matrix, csv_filename);
fprintf('SSIM matrix has been saved to %s\n', csv_filename);

% Optional: Visualize the SSIM matrix as a heatmap
figure;
imagesc(ssim_matrix);
colormap('jet');
colorbar;
axis equal tight;
title('SSIM All-to-All Comparison Heatmap');
xlabel('Image Index');
ylabel('Image Index');