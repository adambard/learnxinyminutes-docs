#!/usr/bin/env ruby
$file_count = 0;
markdown_files = Dir["./**/*.html.markdown"]
markdown_files.each do |file|
  begin
    file_bin = File.open(file, "rb")
    contents = file_bin.read
    if ! contents.valid_encoding?
      puts "#{file} has an invalid encoding! Please save the file in UTF-8!"
    else
      $file_count = $file_count + 1
    end
  rescue Exception => msg
    puts msg
  end
end
files_failed = markdown_files.length - $file_count
if files_failed != 0
  puts "FAILURE!!! #{files_failed} files were unable to be validated as UTF-8!"
  puts "Please resave the file as UTF-8."
  exit 1
else
  puts "Success. All #{$file_count} files passed UTF-8 validity checks"
  exit 0
end
