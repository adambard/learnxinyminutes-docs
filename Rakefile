task default: %w[test]

task :test do
  Dir["./tests/*.rb"].each do |test_file|
    ruby test_file
  end
end
