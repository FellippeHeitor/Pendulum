CONST true = -1, false = 0
CONST showVars = false

_TITLE "I came in like a wreeeeeecking ball..."

gameWidth = 900
gameHeight = 650

gameScreen = _NEWIMAGE(gameWidth, gameHeight, 32)
SCREEN gameScreen
COLOR , _RGBA32(0, 0, 0, 0)
arena = _NEWIMAGE(gameWidth * 10, gameHeight, 32)
_SOURCE arena
arenaBG = _NEWIMAGE(gameWidth * 20, gameHeight, 32)

CONST FIRE = 1
CONST SMOKE = 2

TYPE newItem
    x AS SINGLE
    y AS SINGLE
    w AS SINGLE
    h AS SINGLE
    yAcc AS SINGLE
    yVel AS SINGLE
    xAcc AS SINGLE
    xVel AS SINGLE
    kind AS INTEGER
    color AS _UNSIGNED LONG
    active AS _BYTE
    generation AS INTEGER
END TYPE

REDIM SHARED block(200) AS newItem
REDIM SHARED particle(200) AS newItem
DIM SHARED totalBlocks AS LONG, totalParticles AS LONG

drawArena

_DEST arena
camera = 0
ball.impulse = 1.005 '.995
ball.radius = 20
ball.y.velocity = 0
ball.y.acceleration = 0
ball.x.velocity = 0
ball.x.acceleration = 0
ball.origin.X = _WIDTH(gameScreen) / 2
ball.origin.Y = 0
ball.x = 0
ball.y = 0

level = 1
levelOSD = TIMER

g = .4

DO
    DO
        ball.y.acceleration = ball.y.acceleration + g / 10
        ball.y.velocity = ball.y.velocity + ball.y.acceleration
        ball.y = ball.y + ball.y.velocity

        'ball.x.velocity = ball.x.velocity + ball.x.acceleration
        ball.x = ball.x + ball.x.velocity

        IF ball.y - ball.radius / 2 > _HEIGHT THEN
            DO
                ball.y = _RED32(POINT(ball.x, 0))
                IF ball.y > 0 THEN EXIT DO
                ball.x = ball.x + 1
            LOOP
            ball.y.acceleration = 0
            ball.y.velocity = 0
            ball.x.acceleration = 0
            ball.x.velocity = 0
        END IF

        FOR p = 1 TO 30
            addParticle ball.x + COS(p) * (RND * ball.radius), ball.y + SIN(p) * (RND * ball.radius), FIRE
        NEXT

        IF ball.x - ball.radius > _WIDTH(arena) THEN madeIt = true: EXIT DO

        IF ball.x + camera > _WIDTH / 3 + _WIDTH / 5 THEN
            camera = (_WIDTH / 3 + _WIDTH / 5) - ball.x
        ELSEIF ball.x + camera < _WIDTH / 3 THEN
            camera = _WIDTH / 3 - ball.x
        END IF

        IF camera > 0 THEN camera = 0
        IF camera < -(_WIDTH(arena) - _WIDTH(gameScreen)) THEN camera = -(_WIDTH(arena) - _WIDTH(gameScreen))

        _DEST arena
        _DONTBLEND
        _PUTIMAGE (camera / 2, 0), arenaBG
        _BLEND
        drawBlocks

        processParticles
        showParticles

        _DEST 0

        _PUTIMAGE (camera, 0), arena
        '_PUTIMAGE (0, _HEIGHT - 40)-(_WIDTH - 1, _HEIGHT - 1), arena 'PIP

        IF TIMER - levelOSD < 1.5 THEN
            m$ = "Level" + STR$(level)
            _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(m$) / 2, _HEIGHT / 2 - _FONTHEIGHT / 2), m$
        END IF
        m$ = STR$(TIMER - levelOSD)
        m$ = LEFT$(m$, INSTR(m$, ".") + 1)
        _PRINTSTRING (_WIDTH - _PRINTWIDTH(m$), _HEIGHT - _FONTHEIGHT), m$

        _DISPLAY
        _LIMIT 60
    LOOP UNTIL _KEYHIT = 13

    ball.y.acceleration = ball.y.acceleration / 50
    ball.y.velocity = ball.y.acceleration
    ball.origin.X = ball.x + _WIDTH(gameScreen) / _CEIL(map(ball.y, 0, _HEIGHT, 6, 4))
    IF ball.origin.X > _WIDTH(arena) THEN
        finished = true
        ball.origin.Y = 0
    ELSE
        DO
            ball.origin.Y = _RED32(POINT(ball.origin.X, 0))
            IF ball.origin.Y > 0 THEN EXIT DO
            ball.origin.X = ball.origin.X + 1
        LOOP
    END IF
    diff.y = ball.origin.Y - ball.y
    diff.x = ball.origin.X - ball.x
    ball.angle = _ATAN2(-1 * diff.y, diff.x) - _D2R(90)
    ball.arm = dist(ball.x, ball.y, ball.origin.X, ball.origin.Y)

    DO
        ball.y.acceleration = (-1 * g / ball.arm) * SIN(ball.angle)
        ball.y.velocity = ball.y.velocity + ball.y.acceleration
        ball.y.velocity = ball.y.velocity * ball.impulse

        ball.angle = ball.angle + ball.y.velocity

        ball.x = ball.origin.X + (ball.arm * SIN(ball.angle))
        ball.y = ball.origin.Y + (ball.arm * COS(ball.angle))

        FOR p = 1 TO 30
            addParticle ball.x + COS(p) * (RND * ball.radius), ball.y + SIN(p) * (RND * ball.radius), FIRE
        NEXT

        IF ball.x + camera > _WIDTH / 3 + _WIDTH / 5 THEN
            camera = (_WIDTH / 3 + _WIDTH / 5) - ball.x
        ELSEIF ball.x + camera < _WIDTH / 3 THEN
            camera = _WIDTH / 3 - ball.x
        END IF

        IF camera > 0 THEN camera = 0
        IF camera < -(_WIDTH(arena) - _WIDTH(gameScreen)) THEN camera = -(_WIDTH(arena) - _WIDTH(gameScreen))

        _DEST arena
        _DONTBLEND
        _PUTIMAGE (camera / 2, 0), arenaBG
        _BLEND

        drawBlocks

        ThickLine ball.x, ball.y, ball.origin.X, ball.origin.Y, _RGB32(0, 0, 0), 8
        ThickLine ball.x, ball.y, ball.origin.X, ball.origin.Y, _RGB32(255, 255, 255), 4

        'CircleFill ball.x, ball.y, ball.radius, _RGB32(255, 255, 255)
        'CircleFill ball.x, ball.y, ball.radius - 2, c

        processParticles
        showParticles

        _DEST 0
        _PUTIMAGE (camera, 0), arena
        '_PUTIMAGE (0, _HEIGHT - 40)-(_WIDTH - 1, _HEIGHT - 1), arena 'PIP

        IF showVars THEN
            LOCATE 1, 1
            PRINT ball.impulse
            PRINT ball.radius
            PRINT ball.y.velocity
            PRINT ball.y.acceleration
            PRINT ball.origin.X
            PRINT ball.origin.Y
            PRINT ball.angle
            PRINT ball.arm
            PRINT ball.x
            PRINT ball.y
            PRINT camera
        END IF

        IF finished THEN
            m$ = "You made it!"
            _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(m$) / 2, _HEIGHT / 2 - _FONTHEIGHT / 2), m$
        ELSE
            IF TIMER - levelOSD < 1.5 THEN
                m$ = "Level" + STR$(level)
                _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(m$) / 2, _HEIGHT / 2 - _FONTHEIGHT / 2), m$
            END IF

            m$ = STR$(TIMER - levelOSD)
            m$ = LEFT$(m$, INSTR(m$, ".") + 1)
            _PRINTSTRING (_WIDTH - _PRINTWIDTH(m$), _HEIGHT - _FONTHEIGHT), m$
        END IF

        _DISPLAY
        _LIMIT 60

        IF NOT finished THEN k = _KEYHIT ELSE k = 0
        IF ball.x - ball.radius > _WIDTH(arena) THEN madeIt = true
    LOOP UNTIL k = -13 OR madeIt

    IF finished OR madeIt THEN
        finished = false
        madeIt = false
        timeFinished = TIMER
        t.m$ = STR$(timeFinished - levelOSD)
        t.m$ = LEFT$(t.m$, INSTR(t.m$, ".") + 1)

        _DEST arena
        _PUTIMAGE (camera / 2, 0), arenaBG
        drawBlocks
        _DEST 0
        _PUTIMAGE (camera, 0), arena

        m$ = "You made it in" + t.m$ + " seconds!"
        _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(m$) / 2, _HEIGHT / 2 - _FONTHEIGHT / 2), m$
        m$ = "(hit space)"
        _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(m$) / 2, _HEIGHT / 2 - _FONTHEIGHT / 2 + _FONTHEIGHT), m$

        DO UNTIL _KEYHIT = 32: _DISPLAY: _LIMIT 30: LOOP

        camera = 0
        ball.x = 0
        ball.y = 0
        ball.y.acceleration = 0
        ball.y.velocity = 0
        ball.x.acceleration = 0
        ball.x.velocity = 0
        level = level + 1
        levelOSD = TIMER
        drawArena
    ELSE
        mag = ball.y.velocity * 1000
        IF mag > 10 THEN mag = 10
        IF mag < -10 THEN mag = -10
        ball.x.velocity = COS(ball.angle) * mag
        ball.y.velocity = -SIN(ball.angle) * mag
    END IF
LOOP

FUNCTION dist! (x1!, y1!, x2!, y2!)
    dist! = _HYPOT((x2! - x1!), (y2! - y1!))
END FUNCTION

FUNCTION map! (value!, minRange!, maxRange!, newMinRange!, newMaxRange!)
    map! = ((value! - minRange!) / (maxRange! - minRange!)) * (newMaxRange! - newMinRange!) + newMinRange!
END FUNCTION

SUB CircleFill (CX AS LONG, CY AS LONG, R AS LONG, C AS _UNSIGNED LONG)
    'This sub from here: http://www.qb64.net/forum/index.php?topic=1848.msg17254#msg17254
    DIM Radius AS LONG
    DIM RadiusError AS LONG
    DIM X AS LONG
    DIM Y AS LONG

    Radius = ABS(R)
    RadiusError = -Radius
    X = Radius
    Y = 0

    IF Radius = 0 THEN PSET (CX, CY), C: EXIT SUB

    ' Draw the middle span here so we don't draw it twice in the main loop,
    ' which would be a problem with blending turned on.
    LINE (CX - X, CY)-(CX + X, CY), C, BF

    WHILE X > Y

        RadiusError = RadiusError + Y * 2 + 1

        IF RadiusError >= 0 THEN

            IF X <> Y + 1 THEN
                LINE (CX - Y, CY - X)-(CX + Y, CY - X), C, BF
                LINE (CX - Y, CY + X)-(CX + Y, CY + X), C, BF
            END IF

            X = X - 1
            RadiusError = RadiusError - X * 2

        END IF

        Y = Y + 1

        LINE (CX - X, CY - Y)-(CX + X, CY - Y), C, BF
        LINE (CX - X, CY + Y)-(CX + X, CY + Y), C, BF

    WEND

END SUB


SUB drawArena
    SHARED arenaBG
    _DEST arenaBG
    _BLEND
    CLS , _RGB32(255, 255, 255)
    FOR i = 1 TO 200
        LINE (RND * _WIDTH, RND * _HEIGHT)-(RND * _WIDTH, RND * _HEIGHT), _RGBA32(RND * 256, RND * 256, RND * 256, RND * 200), BF
    NEXT

    blockSize = 100
    margin = 3
    totalBlocks = 0
    FOR i = 0 TO _WIDTH STEP blockSize
        LINE (i, 0)-STEP(blockSize - 1, _HEIGHT), _RGBA32(0, 0, 0, map(i, 0, _WIDTH, 160, 0)), BF

        'top block
        h = RND * 150 + 50
        y = 0
        GOSUB addBlock

        'LINE (i, y)-STEP(blockSize, h), _RGB32(0, 0, 0), BF
        'LINE (i + margin, y)-STEP(blockSize - (margin * 2), h - margin), _RGB32(h, h, h), BF

        'bottom block
        h = RND * blockSize + 50
        y = _HEIGHT - h
        GOSUB addBlock

        'LINE (i, y)-STEP(blockSize, -h), _RGB32(0, 0, 0), BF
        'LINE (i + margin, _HEIGHT)-STEP(blockSize - (margin * 2), -(h - margin)), _RGB32(h, h, h), BF
    NEXT
    _DONTBLEND
    EXIT SUB

    addBlock:
    totalBlocks = totalBlocks + 1
    IF totalBlocks > UBOUND(block) THEN REDIM _PRESERVE block(UBOUND(block) + 99) AS newItem
    block(totalBlocks).x = i
    block(totalBlocks).y = y
    block(totalBlocks).h = h
    block(totalBlocks).w = blockSize
    RETURN
END SUB

SUB ThickLine (x, y, x1, y1, c AS _UNSIGNED LONG, t)
    'This sub from http://www.qb64.net/forum/index.php?topic=1456.msg11548#msg11548
    FOR i = 0 TO 1 STEP .01
        xx = INT((x1 - x) * i) + x
        yy = INT((y1 - y) * i) + y
        CircleFill xx, yy, t \ 2, c
    NEXT
END SUB

SUB drawBlocks
    SHARED camera
    margin = 3
    loopStart = INT(ABS(camera) / (block(1).w / 2)) - 2
    IF loopStart < 1 THEN loopStart = 1
    FOR j = loopStart TO totalBlocks
        i = block(j).x
        IF i + camera > _WIDTH(0) THEN EXIT FOR
        y = block(j).y
        h = block(j).h
        blocksize = block(j).w
        LINE (i, y - margin)-STEP(blocksize, h + margin), _RGB32(0, 0, 0), BF
        LINE (i + margin, y)-STEP(blocksize - (margin * 2), h - margin), _RGB32(h, h, h), BF
        IF showVars THEN _PRINTSTRING (i, y), STR$(j)
    NEXT
END SUB

SUB showParticles
    SHARED ball.x, ball.y, ball.radius
    FOR i = 1 TO UBOUND(particle)
        IF particle(i).active THEN
            SELECT CASE particle(i).kind
                CASE FIRE, SMOKE
                    s = dist(particle(i).x, particle(i).y, ball.x, ball.y)
                    s = map(s, 0, ball.radius * 4, 9, 2)
                    IF s < 2 THEN s = 2
                    CircleFill particle(i).x, particle(i).y, s, particle(i).color
            END SELECT
        END IF
    NEXT
END SUB

SUB addParticle (x AS SINGLE, y AS SINGLE, kind AS INTEGER)
    DIM i AS LONG

    IF totalParticles > 5000 THEN EXIT SUB

    FOR i = 1 TO UBOUND(particle)
        IF NOT particle(i).active THEN newParticle = i: EXIT FOR
    NEXT

    IF newParticle = 0 THEN
        totalParticles = totalParticles + 1
        newParticle = totalParticles
        IF totalParticles > UBOUND(particle) THEN
            REDIM _PRESERVE particle(1 TO UBOUND(particle) + 99) AS newItem
        END IF
    END IF

    particle(newParticle).x = x
    particle(newParticle).xVel = 0
    particle(newParticle).y = y
    particle(newParticle).yVel = 0
    particle(newParticle).kind = kind
    particle(newParticle).generation = 0
    particle(newParticle).active = true

    SELECT CASE kind
        CASE FIRE
            a = _D2R(RND * 360)
            particle(newParticle).yVel = SIN(a) * 2
            particle(newParticle).xVel = COS(a) * 2
    END SELECT
END SUB

SUB processParticles
    DIM i AS LONG
    FOR i = 1 TO UBOUND(particle)
        IF particle(i).active THEN
            particle(i).generation = particle(i).generation + 1

            particle(i).yVel = particle(i).yVel + particle(i).yAcc
            particle(i).y = particle(i).y + particle(i).yVel

            particle(i).xVel = particle(i).xVel + particle(i).xAcc
            particle(i).x = particle(i).x + particle(i).xVel

            SELECT CASE particle(i).kind
                CASE FIRE
                    IF particle(i).generation > 8 THEN
                        particle(i).kind = SMOKE
                        particle(i).generation = 0
                        particle(i).yVel = 0
                        particle(i).xVel = 0
                        particle(i).yAcc = -.4
                        particle(i).xAcc = -.2
                    ELSE
                        SELECT CASE particle(i).generation
                            CASE 1, 2: g = 238: b = 177: a = 255
                            CASE 3, 4: g = 222: b = 89: a = 200
                            CASE 5, 6: g = 128: b = 50: a = 150
                            CASE 7, 8: g = 33: b = 0: a = 70
                        END SELECT
                        particle(i).color = _RGBA32(255, g, b, a)
                    END IF
                CASE SMOKE
                    maxSmoke = 10
                    particle(i).color = _RGBA32(33, 17, 39, map(particle(i).generation, 1, maxSmoke, 200, 0))
                    IF particle(i).generation > maxSmoke THEN particle(i).active = false
            END SELECT

        END IF
    NEXT
END SUB
