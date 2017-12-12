OPTION _EXPLICIT

CONST true = -1, false = 0
CONST showVars = false

_TITLE "I came in like a wreeeeeecking ball..."

DIM SHARED gameWidth AS INTEGER, gameHeight AS INTEGER
DIM SHARED gameScreen AS LONG, arena AS LONG, arenaBG AS LONG
DIM SHARED level AS LONG, camera AS SINGLE
DIM SHARED blockOffset AS INTEGER, respawnOffset AS SINGLE
DIM SHARED ball.impulse, ball.radius, ball.y.velocity
DIM SHARED ball.y.acceleration, ball.x.velocity, ball.x.acceleration
DIM SHARED ball.origin.x, ball.origin.y, ball.x, ball.y
DIM SHARED level.g, level.b, g, k AS LONG
DIM SHARED started AS _BYTE, p AS INTEGER, tx, ty, madeIt AS _BYTE
DIM SHARED cameraCenter AS SINGLE, cameraCenterY AS SINGLE, cameraY AS SINGLE
DIM SHARED timerSet AS _BYTE, m$, levelStarted AS SINGLE, finished AS _BYTE
DIM SHARED diff.y, diff.x, ball.angle, ball.arm, timeFinished AS SINGLE
DIM SHARED t.m$, mag

gameWidth = 900
gameHeight = 650

gameScreen = _NEWIMAGE(gameWidth, gameHeight, 32)
SCREEN gameScreen
COLOR , _RGBA32(0, 0, 0, 0)
arena = _NEWIMAGE(gameWidth * 15, gameHeight * 1.5, 32)
_SOURCE arena
arenaBG = _NEWIMAGE(gameWidth * 30, gameHeight * 1.5, 32)

CONST FIRE = 1
CONST SMOKE = 2
CONST SPARK = 3

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
DIM SHARED activeParticles AS LONG, activeSparks AS LONG

CONST maxRND = 1000000
DIM SHARED rndTable(1 TO maxRND) AS SINGLE
DIM SHARED rndSeed AS LONG, rndIndex AS LONG
RANDOMIZE 17
DIM i&
FOR i& = 1 TO maxRND
    rndTable(i&) = RND
NEXT

level = 1
setRand level
drawArena

_DEST arena
camera = 0
blockOffset = 200
respawnOffset = 1.9
ball.impulse = 1.003 '.damping = .995
ball.radius = 20
ball.y.velocity = 0
ball.y.acceleration = 0
ball.x.velocity = 0
ball.x.acceleration = 0
ball.origin.x = _WIDTH(gameScreen) / 2
ball.origin.y = 0
ball.x = ball.radius * 1.5
ball.y = _HEIGHT(arena) / respawnOffset

level.g = getRND * 256
level.b = getRND * 256

DO
    doPhysics
    g = .1
    DO
        k = _KEYHIT
        IF k = 9 THEN GOTO setNewLevel
        IF k = 25 AND level > 1 THEN level = level - 2: GOTO setNewLevel
        IF started THEN
            ball.y.acceleration = ball.y.acceleration + g / 10
            ball.y.velocity = ball.y.velocity + ball.y.acceleration
            ball.y = ball.y + ball.y.velocity

            'ball.x.velocity = ball.x.velocity + ball.x.acceleration
            ball.x = ball.x + ball.x.velocity

            IF ball.y - ball.radius / 2 > _HEIGHT(arena) THEN
                'DO
                '    ball.y = _RED32(POINT(ball.x, 0))
                '    IF ball.y > 0 THEN EXIT DO
                '    ball.x = ball.x + 1
                'LOOP
                ball.y = _HEIGHT(arena) / respawnOffset
                started = false
                ball.y.acceleration = 0
                ball.y.velocity = 0
                ball.x.acceleration = 0
                ball.x.velocity = 0
            END IF
        END IF

        FOR p = 1 TO 30
            tx = ball.x + COS(p) * (getRND * ball.radius)
            ty = ball.y + SIN(p) * (getRND * ball.radius)
            addParticle tx, ty, FIRE
        NEXT

        IF ball.x - ball.radius > _WIDTH(arena) THEN madeIt = true: EXIT DO

        cameraCenter = 3
        cameraCenterY = 2

        IF ball.x + camera > _WIDTH / cameraCenter THEN
            camera = (_WIDTH / cameraCenter) - ball.x
        ELSEIF ball.x + camera < _WIDTH / cameraCenter THEN
            camera = _WIDTH / cameraCenter - ball.x
        END IF

        IF ball.y + cameraY > _HEIGHT / cameraCenterY THEN
            cameraY = (_HEIGHT / cameraCenterY) - ball.y
        ELSEIF ball.y + cameraY < _HEIGHT / cameraCenterY THEN
            cameraY = _HEIGHT / cameraCenterY - ball.y
        END IF

        IF camera > 0 THEN camera = 0
        IF camera < -(_WIDTH(arena) - _WIDTH(gameScreen)) THEN camera = -(_WIDTH(arena) - _WIDTH(gameScreen))

        IF cameraY > 0 THEN cameraY = 0
        IF cameraY < -(_HEIGHT(arena) - _HEIGHT(gameScreen)) THEN cameraY = -(_HEIGHT(arena) - _HEIGHT(gameScreen))


        _DEST arena
        _DONTBLEND
        _PUTIMAGE (camera / 2, 0), arenaBG
        _BLEND
        drawBlocks

        doParticles

        _DEST 0

        _PUTIMAGE (camera, cameraY), arena
        '_PUTIMAGE (0, _HEIGHT - 40)-(_WIDTH - 1, _HEIGHT - 1), arena 'PIP

        IF NOT started THEN
            IF NOT timerSet THEN m$ = "Hold ENTER to start..." ELSE m$ = "Hold ENTER to continue..."
            _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(m$) / 2, _HEIGHT / 2 - _FONTHEIGHT / 2), m$
        END IF

        IF timerSet THEN
            m$ = STR$(TIMER - levelStarted)
            m$ = LEFT$(m$, INSTR(m$, ".") + 1)
            _PRINTSTRING (_WIDTH - _PRINTWIDTH(m$), _HEIGHT - _FONTHEIGHT), m$
        END IF

        _PRINTSTRING (0, 0), STR$(activeParticles)
        _PRINTSTRING (0, 20), STR$(activeSparks)
        _PRINTSTRING (0, 40), STR$(rndIndex)

        _DISPLAY
        _LIMIT 60
    LOOP WHILE _KEYDOWN(13) = false

    started = true
    g = .9
    IF timerSet = false THEN
        timerSet = true
        levelStarted = TIMER
    END IF

    ball.y.acceleration = ball.y.acceleration / 25
    ball.y.velocity = ball.y.acceleration
    ball.origin.x = ball.x + _WIDTH(gameScreen) / _CEIL(map(ball.y, 0, _HEIGHT, 6, 4))
    IF ball.origin.x > _WIDTH(arena) THEN
        finished = true
        ball.origin.y = 0
    ELSE
        DO
            ball.origin.y = _RED32(POINT(ball.origin.x, 0))
            IF ball.origin.y > 0 THEN EXIT DO
            ball.origin.x = ball.origin.x + 1
        LOOP
    END IF
    ball.origin.y = ball.origin.y + blockOffset
    diff.y = ball.origin.y - ball.y
    diff.x = ball.origin.x - ball.x
    ball.angle = _ATAN2(-1 * diff.y, diff.x) - _D2R(90)
    ball.arm = dist(ball.x, ball.y, ball.origin.x, ball.origin.y)

    DO
        ball.y.acceleration = (-1 * g / ball.arm) * SIN(ball.angle)
        ball.y.velocity = ball.y.velocity + ball.y.acceleration
        ball.y.velocity = ball.y.velocity * ball.impulse

        ball.angle = ball.angle + ball.y.velocity

        ball.x = ball.origin.x + (ball.arm * SIN(ball.angle))
        ball.y = ball.origin.y + (ball.arm * COS(ball.angle))

        FOR p = 1 TO 30
            tx = ball.x + COS(p) * (getRND * ball.radius)
            ty = ball.y + SIN(p) * (getRND * ball.radius)
            addParticle tx, ty, FIRE
        NEXT

        IF ball.x + camera > _WIDTH / cameraCenter THEN
            camera = (_WIDTH / cameraCenter) - ball.x
        ELSEIF ball.x + camera < _WIDTH / cameraCenter THEN
            camera = _WIDTH / cameraCenter - ball.x
        END IF

        IF ball.y + cameraY > _HEIGHT / cameraCenterY THEN
            cameraY = (_HEIGHT / cameraCenterY) - ball.y
        ELSEIF ball.y + cameraY < _HEIGHT / cameraCenterY THEN
            cameraY = _HEIGHT / cameraCenterY - ball.y
        END IF

        IF camera > 0 THEN camera = 0
        IF camera < -(_WIDTH(arena) - _WIDTH(gameScreen)) THEN camera = -(_WIDTH(arena) - _WIDTH(gameScreen))

        IF cameraY > 0 THEN cameraY = 0
        IF cameraY < -(_HEIGHT(arena) - _HEIGHT(gameScreen)) THEN cameraY = -(_HEIGHT(arena) - _HEIGHT(gameScreen))

        _DEST arena
        _DONTBLEND
        _PUTIMAGE (camera / 2, 0), arenaBG
        _BLEND

        drawBlocks

        ThickLine ball.x, ball.y, ball.origin.x, ball.origin.y, _RGB32(0, 0, 0), 8
        ThickLine ball.x, ball.y, ball.origin.x, ball.origin.y, _RGB32(255, 255, 255), 4

        doParticles

        _DEST 0
        _PUTIMAGE (camera, cameraY), arena
        '_PUTIMAGE (0, _HEIGHT - 40)-(_WIDTH - 1, _HEIGHT - 1), arena 'PIP

        IF showVars THEN
            LOCATE 1, 1
            PRINT ball.impulse
            PRINT ball.radius
            PRINT ball.y.velocity
            PRINT ball.y.acceleration
            PRINT ball.origin.x
            PRINT ball.origin.y
            PRINT ball.angle
            PRINT ball.arm
            PRINT ball.x
            PRINT ball.y
            PRINT camera, cameraY
        END IF

        IF finished THEN
            m$ = "You made it!"
            _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(m$) / 2, _HEIGHT / 2 - _FONTHEIGHT / 2), m$
        ELSE
            IF started THEN
                m$ = STR$(TIMER - levelStarted)
                m$ = LEFT$(m$, INSTR(m$, ".") + 1)
                _PRINTSTRING (_WIDTH - _PRINTWIDTH(m$), _HEIGHT - _FONTHEIGHT), m$
            END IF
        END IF

        _PRINTSTRING (0, 0), STR$(activeParticles)
        _PRINTSTRING (0, 20), STR$(activeSparks)
        _PRINTSTRING (0, 40), STR$(rndIndex)

        _DISPLAY
        _LIMIT 60

        IF NOT finished THEN k = (_KEYDOWN(13) = false) ELSE k = 0
        IF ball.x - ball.radius > _WIDTH(arena) THEN madeIt = true
    LOOP UNTIL k OR madeIt

    IF finished OR madeIt THEN
        timeFinished = TIMER
        t.m$ = STR$(timeFinished - levelStarted)
        t.m$ = LEFT$(t.m$, INSTR(t.m$, ".") + 1)

        DO
            IF activeParticles THEN
                _DEST arena
                _DONTBLEND
                _PUTIMAGE (camera / 2, 0), arenaBG
                _BLEND
                drawBlocks
                doParticles
                _DEST 0
                _PUTIMAGE (camera, cameraY), arena

                m$ = "You made it in" + t.m$ + " seconds!"
                _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(m$) / 2, _HEIGHT / 2 - _FONTHEIGHT / 2), m$
                m$ = "(hit space)"
                _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(m$) / 2, _HEIGHT / 2 - _FONTHEIGHT / 2 + _FONTHEIGHT), m$
            END IF

            k = _KEYHIT

            _DISPLAY
            _LIMIT 60
        LOOP UNTIL k = 32

        setNewLevel:
        finished = false
        madeIt = false
        camera = 0
        cameraY = 0
        ball.x = ball.radius
        ball.y = _HEIGHT(arena) / 1.5
        ball.y.acceleration = 0
        ball.y.velocity = 0
        ball.x.acceleration = 0
        ball.x.velocity = 0
        level = level + 1
        setRand level
        drawArena
        started = false
        timerSet = false
        level.g = getRND * 256
        level.b = getRND * 256
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
    DIM i AS LONG, blockSize AS INTEGER
    DIM h AS SINGLE, y AS SINGLE
    STATIC loadingHUD

    IF loadingHUD = 0 THEN
        loadingHUD = _NEWIMAGE(_WIDTH(0) / 2, _HEIGHT(0) / 2, 32)
    END IF

    _DEST loadingHUD
    CLS , 0
    m$ = "Level" + STR$(level)
    COLOR , 0
    _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(m$) / 2, _HEIGHT / 2 - _FONTHEIGHT / 2), m$

    _DEST arenaBG
    _BLEND
    CLS , _RGB32(255, 255, 255)
    FOR i = 1 TO 25
        LINE (getRND * _WIDTH, getRND * _HEIGHT)-(getRND * _WIDTH, getRND * _HEIGHT), _RGBA32(getRND * 256, getRND * 256, getRND * 256, getRND * 200), BF
        _DONTBLEND
        _PUTIMAGE , arenaBG, _DISPLAY
        _PUTIMAGE , loadingHUD, _DISPLAY
        _BLEND
        _DISPLAY
    NEXT

    blockSize = 100
    FOR i = 0 TO _WIDTH STEP blockSize
        LINE (i, 0)-STEP(blockSize - 1, _HEIGHT), _RGBA32(0, 0, 0, map(i, 0, _WIDTH, 160, 50)), BF
    NEXT

    totalBlocks = 0
    FOR i = 0 TO _WIDTH(arena) STEP blockSize
        'top block
        h = getRND * 256 '206 + 50
        y = 0
        GOSUB addBlock

        'bottom block
        h = getRND * 256 '206 + 50
        y = _HEIGHT(arena) - h
        GOSUB addBlock
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
    DIM i AS SINGLE, xx AS SINGLE, yy AS SINGLE
    'This sub from http://www.qb64.net/forum/index.php?topic=1456.msg11548#msg11548
    FOR i = 0 TO 1 STEP .01
        xx = INT((x1 - x) * i) + x
        yy = INT((y1 - y) * i) + y
        CircleFill xx, yy, t \ 2, c
    NEXT
END SUB

SUB drawBlocks
    DIM margin, loopStart AS LONG, j AS LONG
    DIM i AS SINGLE, y AS SINGLE, h AS SINGLE
    DIM blockSize AS SINGLE

    margin = 3
    loopStart = INT(ABS(camera) / (block(1).w / 2)) - 2
    IF loopStart < 1 THEN loopStart = 1
    FOR j = loopStart TO totalBlocks
        i = block(j).x
        IF i + camera > _WIDTH(0) THEN EXIT FOR
        y = block(j).y
        h = block(j).h
        blockSize = block(j).w
        LINE (i, y - margin)-STEP(blockSize, blockOffset + h + margin), _RGB32(0, 0, 0), BF
        LINE (i + margin, y)-STEP(blockSize - (margin * 2), blockOffset + h - margin), _RGB32(h, level.g, level.b), BF
        IF showVars THEN _PRINTSTRING (i, y), STR$(j)
    NEXT
END SUB

SUB addParticle (x AS SINGLE, y AS SINGLE, kind AS INTEGER)
    DIM i AS LONG
    DIM newParticle AS LONG
    DIM a AS SINGLE
    DIM s AS SINGLE

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
    particle(newParticle).xAcc = 0
    particle(newParticle).y = y
    particle(newParticle).yVel = 0
    particle(newParticle).yAcc = 0
    particle(newParticle).kind = kind
    particle(newParticle).generation = 0
    particle(newParticle).active = true

    a = getRND * _PI(2)

    SELECT CASE kind
        CASE FIRE
            particle(newParticle).yVel = SIN(a) * 2
            particle(newParticle).xVel = COS(a) * 2
        CASE SPARK
            particle(newParticle).yVel = SIN(a) * 3
            particle(newParticle).xVel = COS(a) * 3
    END SELECT
END SUB

SUB doParticles
    SHARED camera
    DIM i AS LONG
    DIM gravity AS SINGLE
    DIM g AS _UNSIGNED _BYTE, b AS _UNSIGNED _BYTE, a AS _UNSIGNED _BYTE
    DIM maxSmoke AS INTEGER, maxSpark AS INTEGER
    DIM s AS SINGLE

    activeParticles = 0
    activeSparks = 0
    gravity = .02
    FOR i = 1 TO UBOUND(particle)
        IF particle(i).active THEN
            activeParticles = activeParticles + 1

            particle(i).generation = particle(i).generation + 1

            IF particle(i).kind = SPARK THEN activeSparks = activeSparks + 1: particle(i).yAcc = particle(i).yAcc + gravity
            particle(i).yVel = particle(i).yVel + particle(i).yAcc
            particle(i).y = particle(i).y + particle(i).yVel

            particle(i).xVel = particle(i).xVel + particle(i).xAcc
            particle(i).x = particle(i).x + particle(i).xVel

            SELECT EVERYCASE particle(i).kind
                CASE FIRE
                    IF particle(i).generation > 8 THEN
                        particle(i).kind = SMOKE
                        particle(i).generation = 0
                        particle(i).yVel = 0
                        particle(i).xVel = 0
                        particle(i).yAcc = -.4
                        particle(i).xAcc = -.2
                    ELSE
                        SELECT EVERYCASE particle(i).generation
                            CASE 1, 2: g = 238: b = 177: a = 200
                            CASE 3
                                s = getRND
                                IF (started AND s < .08) THEN
                                    addParticle particle(i).x, particle(i).y, SPARK
                                ELSEIF NOT started THEN
                                    IF s < .01 THEN addParticle particle(i).x, particle(i).y, SPARK
                                END IF
                            CASE 3, 4: g = 222: b = 89: a = 180
                            CASE 5, 6: g = 128: b = 50: a = 150
                            CASE 7, 8: g = 33: b = 0: a = 70
                        END SELECT
                        particle(i).color = _RGBA32(255, g, b, a)
                    END IF
                CASE SMOKE
                    maxSmoke = 15
                    particle(i).color = _RGBA32(33, 17, 39, map(particle(i).generation, 1, maxSmoke, 100, 0))
                    IF particle(i).generation > maxSmoke THEN particle(i).active = false
                CASE SPARK
                    maxSpark = 25
                    SELECT CASE particle(i).generation
                        CASE 1, 2: g = 238: b = 177
                        CASE 3, 4: g = 222: b = 89
                        CASE 5, 6: g = 128: b = 50
                        CASE 7, 8: g = 33: b = 0
                    END SELECT
                    a = map(particle(i).generation, 1, 15, 255, 0)
                    particle(i).color = _RGBA32(255, g, b, a)
                    IF particle(i).generation > maxSpark THEN particle(i).active = false
                    'end of processing -----------------------------------------------------------


                    'show ------------------------------------------------------------------------
                CASE FIRE, SMOKE
                    s = dist(particle(i).x, particle(i).y, ball.x, ball.y)
                    s = map(s, 0, ball.radius * 4, 9, 2)
                    IF s < 2 THEN s = 2
                    CircleFill particle(i).x, particle(i).y, s, particle(i).color
                CASE SPARK
                    'PSET (particle(i).x, particle(i).y), particle(i).color
                    CircleFill particle(i).x, particle(i).y, 1, particle(i).color
            END SELECT
        END IF
    NEXT
END SUB


SUB setRand (seed&)
    IF seed& > UBOUND(rndtable) OR seed& < 1 THEN
        rndIndex = 1
    ELSE
        rndIndex = seed&
    END IF
END SUB

FUNCTION getRND
    rndIndex = rndIndex + 1
    IF rndIndex > UBOUND(rndtable) THEN rndIndex = 1
    getRND = rndTable(rndIndex)
END FUNCTION

SUB doPhysics

END SUB
