require 'fileutils'
require 'colorize'

# Modified from Jack Danger's to grab everything from the dir besides an excluded list
# because I'd prefer to only add things manually if they need to _not_ be copied
# and also to use fileutils symlink

def safe_symlink(source_list, destination_dir)
  # gotta add the relative destination (after ~/) for the overlap check to work properly
  destination_relative = (destination_dir + "/")[Dir.home.length + 1 .. -1]
  already_exists = Dir.entries(destination_dir).map { |file| "#{destination_relative}#{file}" } 
  overlap = source_list & already_exists

  unless overlap.empty?
    puts "Following files already exist, skipping them:".green
    puts overlap
    puts 
  end

  to_link = (source_list - overlap).map { |file| File.expand_path file }

  unless to_link.empty?
    puts "Actual command here".red
    symlink to_link, destination_dir, verbose: true
    puts
  end
  
end


dont_include = [
  'Brewfile',
  'README.md',
  'Rakefile',
  'aliases',
  'bin',
  'install_rvm',
  'osx',
  'setup',
  'TODO',
  'zprezto',
  '.emacs.d',
  '.git',
  '.gitmodules',
]


task :install do
  # directories to copy over
  emacs_dir = File.join(Dir.home, ".emacs.d")
  bin_dir   = File.join(Dir.home, "bin")
  
  # make the directories if they don't exist
  Dir.mkdir(emacs_dir, 0755) unless File.exist?(emacs_dir)
  Dir.mkdir(bin_dir, 0755)   unless File.exist?(bin_dir)

  # get locations
  dot_files   = Dir.glob("*", File::FNM_DOTMATCH) - %w[. ..] - dont_include
  bin_files   = Dir.glob("bin/*", File::FNM_DOTMATCH) - %w[bin/. bin/..]
  emacs_files = Dir.glob(".emacs.d/*", File::FNM_DOTMATCH) - %w[.emacs.d/. .emacs.d/..]
  
  safe_symlink dot_files,   Dir.home
  safe_symlink emacs_files, emacs_dir
  safe_symlink bin_files,   bin_dir
end

task :default => :install
