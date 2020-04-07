def main():
    def a():
        b()
    end
    def b():
        a()
    end
end