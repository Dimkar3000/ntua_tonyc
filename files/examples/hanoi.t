def solve ():
    def hanoi (int rings; char[] source, target, auxiliary):

        def move (char[] source, target):
            puts(”Moving from ”) puts(source) puts(” to ”) puts(target) puts(”.\n”)
        end

        if rings >= 1:
            hanoi(rings-1, source, auxiliary, target)
            move(source, target)
            hanoi(rings-1, auxiliary, target, source)
        end
    end

    int NumberOfRings

    puts(”Rings: ”)
    NumberOfRings := readInteger()
    hanoi(NumberOfRings, ”left”, ”right”, ”middle”)
end