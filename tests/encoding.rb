#!/usr/bin/env ruby
require 'charlock_holmes'
$file_count = 0;
markdown_files = Dir["./**/*.html.markdown"]
markdown_files.each do |file|
  begin
    contents = File.read(file)
    detection = CharlockHolmes::EncodingDetector.detect(contents)
    case detection[:encoding]
    when 'UTF-8'
      $file_count = $file_count + 1
    when 'ISO-8859-1'
      $file_count = $file_count + 1
    else
      puts "#{file} was detected as #{detection[:encoding]} encoding! Please save the file in UTF-8!"
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
  puts "Success. All #{$file_count} files Ruby's UTF-8 validity checks. This won't catch most problems."
  exit 0
end
