#! /usr/bin/env ruby

require "rubygems/package"

def pandoc(from_format, to_format, strings)
  cmd = ["pandoc-tar", "--from", from_format, "--to", to_format]
  IO.popen(cmd, "w+") do |subprocess|
    Gem::Package::TarWriter.new(subprocess) do |tar|
      strings.each_with_index do |string, i|
        tar.add_file_simple("#{i}", 0, string.length) do |output|
          output.write(string)
        end
      end
    end
    subprocess.close_write
    results = []
    Gem::Package::TarReader.new(StringIO.new(subprocess.read)) do |tar|
      tar.each do |entry|
        results.push(if entry.file? then entry.read else nil end)
      end
    end
    results
  end
end

MARKDOWN = ["# Hello world", "* foo\n* bar"]
pandoc("markdown", "html", MARKDOWN).each do |s| puts s end
