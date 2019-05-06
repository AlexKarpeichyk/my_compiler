@.main.x = private constant [21 x i8] c"It's been an honour\0A\00"
@.main.y = private constant [30 x i8] c"to be under your supervision\0A\00"
@.main.z = private constant [5 x i8] c"Sir\0A\00"
@.IntegerPrint = private constant [4 x i8] c"%d\0A\00"
@True = private constant [6 x i8] c"true\0A\00"
@False = private constant [7 x i8] c"false\0A\00"

declare i32 @printf(i8*, ...)

define void @bool_print(i1 %x)
{
entry:
	%bl = icmp eq i1 %x, 1
	br i1 %bl, label %truePrint, label %falsePrint
falsePrint:
	%False = getelementptr [7 x i8], [7 x i8]* @False, i32 0, i32 0
	call i32 (i8*, ...) @printf(i8* %False)
	br label %cont
truePrint:
	%True = getelementptr [6 x i8], [6 x i8]* @True, i32 0, i32 0
	call i32 (i8*, ...) @printf(i8* %True)
	br label %cont
cont:
	ret void
}



define void @foo(i32 %x, i8* %y, i8* %z)
{
	%if.1 = icmp eq i32 %x, 0
	br i1 %if.1, label %then.1, label %el.1
then.1:
	call i32 (i8*, ...) @printf(i8* %y)
	br label %cont.1
el.1:
	call i32 (i8*, ...) @printf(i8* %z)
	br label %cont.1
cont.1:
	
	ret void
}

define i32 @main()
{
	%x = getelementptr [21 x i8],[21 x i8]* @.main.x, i32 0, i32 0
	%y = getelementptr [30 x i8],[30 x i8]* @.main.y, i32 0, i32 0
	%1 = alloca i32, align 4
	store i32 0, i32* %1, align 4
	%xi = load i32, i32* %1, align 4
	call void @foo(i32 %xi, i8* %x, i8* %y)
	%2 = alloca i32, align 4
	%3 = add i32 %xi, 1
	store i32 %3, i32* %2, align 4
	%yi = load i32, i32* %2, align 4
	call void @foo(i32 %yi, i8* %x, i8* %y)
	%z = getelementptr [5 x i8],[5 x i8]* @.main.z, i32 0, i32 0
	call i32 (i8*, ...) @printf(i8* %z)
	
	ret i32 0
}
