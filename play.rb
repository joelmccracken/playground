puts "starting to play!"


playgrounds = [
  { :name => "SICP",
    :directory => "sicp/"},
  { :name => "metaprogramming_ruby",
    :directory => "metaprogramming_ruby/"},
  { :name => "ruby_macros",
    :directory => "ruby_macros/"},
  { :name => "programming_language_design",
    :directory => "programming_language_design/"},
  { :name => "project_euler",
    :directory => "project_euler/"},
]

playgrounds.each do |pground|
  load "#{pground[:directory]}/play_#{pground[:name]}.rb"
end
