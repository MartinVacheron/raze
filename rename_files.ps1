# Define the root directory
$rootDir = ".\tests"

# Get all .rev files in the directory and its subdirectories
$files = Get-ChildItem -Path $rootDir -Recurse -Filter "*.rev"

foreach ($file in $files) {
    # Define the new file name
    $newFileName = [System.IO.Path]::ChangeExtension($file.FullName, ".rz")

    # Rename the file
    Rename-Item -Path $file.FullName -NewName $newFileName
}

Write-Host "Renaming completed!"
