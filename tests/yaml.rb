#!/usr/bin/env ruby
require 'yaml';
$file_count = 0;
markdown_files = Dir["./**/*.html.markdown"]
markdown_files.each do |file|
	begin
		YAML.load_file(file)
		$file_count = $file_count + 1
	rescue Exception => msg
		puts msg	
	end
end
files_failed = markdown_files.length - $file_count
if files_failed != 0
	puts "FAILURE!!! #{files_failed} files were unable to be parsed!"
	puts "Please check the YAML headers for the documents that failed!"
	exit 1
else
	puts "Success. All #{$file_count} were checked"
	exit 0
end
