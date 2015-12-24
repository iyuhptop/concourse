package db_test

import (
	"database/sql"
	"time"

	"github.com/lib/pq"
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
	"github.com/pivotal-golang/lager/lagertest"

	"github.com/concourse/atc"
	"github.com/concourse/atc/db"
)

var _ = Describe("Keeping track of workers", func() {
	var dbConn *sql.DB
	var listener *pq.Listener

	var database db.DB

	BeforeEach(func() {
		postgresRunner.Truncate()

		dbConn = postgresRunner.Open()
		listener = pq.NewListener(postgresRunner.DataSourceName(), time.Second, time.Minute, nil)

		Eventually(listener.Ping, 5*time.Second).ShouldNot(HaveOccurred())
		bus := db.NewNotificationsBus(listener, dbConn)

		database = db.NewSQL(lagertest.NewTestLogger("test"), dbConn, bus)
	})

	AfterEach(func() {
		err := dbConn.Close()
		Expect(err).NotTo(HaveOccurred())

		err = listener.Close()
		Expect(err).NotTo(HaveOccurred())
	})

	It("can keep track of workers", func() {
		Expect(database.Workers()).To(BeEmpty())

		infoA := db.WorkerInfo{
			Name:             "workerName1",
			GardenAddr:       "1.2.3.4:7777",
			BaggageclaimURL:  "5.6.7.8:7788",
			ActiveContainers: 42,
			ResourceTypes: []atc.WorkerResourceType{
				{Type: "some-resource-a", Image: "some-image-a"},
			},
			Platform: "webos",
			Tags:     []string{"palm", "was", "great"},
		}

		infoB := db.WorkerInfo{
			GardenAddr:       "1.2.3.4:8888",
			ActiveContainers: 42,
			ResourceTypes: []atc.WorkerResourceType{
				{Type: "some-resource-b", Image: "some-image-b"},
			},
			Platform: "plan9",
			Tags:     []string{"russ", "cox", "was", "here"},
		}
		expectedSavedWorkerA := db.SavedWorker{
			WorkerInfo: infoA,
			ExpiresIn:  0,
		}

		By("persisting workers with no TTLs")
		savedWorkerA, err := database.SaveWorker(infoA, 0)
		Expect(err).NotTo(HaveOccurred())
		expectedSavedWorkerA.ID = savedWorkerA.ID

		Expect(database.Workers()).To(ConsistOf(expectedSavedWorkerA))

		By("being idempotent")
		_, err = database.SaveWorker(infoA, 0)
		Expect(err).NotTo(HaveOccurred())

		Expect(database.Workers()).To(ConsistOf(expectedSavedWorkerA))

		By("updating attributes by name")
		infoA.GardenAddr = "1.2.3.4:9876"
		expectedSavedWorkerA.WorkerInfo = infoA

		_, err = database.SaveWorker(infoA, 0)
		Expect(err).NotTo(HaveOccurred())

		Expect(database.Workers()).To(ConsistOf(expectedSavedWorkerA))

		By("updating attributes by address")
		infoA.Name = "someNewName"
		expectedSavedWorkerA.WorkerInfo = infoA

		_, err = database.SaveWorker(infoA, 0)
		Expect(err).NotTo(HaveOccurred())

		Expect(database.Workers()).To(ConsistOf(expectedSavedWorkerA))

		By("expiring TTLs")
		ttl := 1 * time.Second

		_, err = database.SaveWorker(infoB, ttl)
		Expect(err).NotTo(HaveOccurred())

		// name is defaulted to addr
		infoBFromDB := infoB
		infoBFromDB.Name = "1.2.3.4:8888"

		workerInfos := func() []db.WorkerInfo {
			return getWorkerInfos(database.Workers())
		}

		Consistently(workerInfos, ttl/2).Should(ConsistOf(infoA, infoBFromDB))
		Eventually(workerInfos, 2*ttl).Should(ConsistOf(infoA))
		By("overwriting TTLs")
		_, err = database.SaveWorker(infoA, ttl)
		Expect(err).NotTo(HaveOccurred())

		Consistently(workerInfos, ttl/2).Should(ConsistOf(infoA))
		Eventually(workerInfos, 2*ttl).Should(BeEmpty())

		By("updating attributes by name with ttls")
		ttl = 1 * time.Hour
		_, err = database.SaveWorker(infoA, ttl)
		Expect(err).NotTo(HaveOccurred())
		Expect(getWorkerInfos(database.Workers())).To(ConsistOf(infoA))

		infoA.GardenAddr = "1.2.3.4:1234"

		_, err = database.SaveWorker(infoA, ttl)
		Expect(err).NotTo(HaveOccurred())

		Expect(getWorkerInfos(database.Workers())).To(ConsistOf(infoA))
	})

	It("it can keep track of a worker", func() {
		By("returning empty worker when worker doesn't exist")
		savedWorker, found, err := database.GetWorker(404)
		Expect(err).NotTo(HaveOccurred())
		Expect(savedWorker).To(Equal(db.SavedWorker{}))
		Expect(found).To(BeFalse())

		infoA := db.WorkerInfo{
			GardenAddr:       "1.2.3.4:7777",
			BaggageclaimURL:  "http://5.6.7.8:7788",
			ActiveContainers: 42,
			ResourceTypes: []atc.WorkerResourceType{
				{Type: "some-resource-a", Image: "some-image-a"},
			},
			Platform: "webos",
			Tags:     []string{"palm", "was", "great"},
			Name:     "workerName1",
		}

		infoB := db.WorkerInfo{
			GardenAddr:       "1.2.3.4:8888",
			BaggageclaimURL:  "http://5.6.7.8:8899",
			ActiveContainers: 42,
			ResourceTypes: []atc.WorkerResourceType{
				{Type: "some-resource-b", Image: "some-image-b"},
			},
			Platform: "plan9",
			Tags:     []string{"russ", "cox", "was", "here"},
			Name:     "workerName2",
		}

		infoC := db.WorkerInfo{
			GardenAddr:       "1.2.3.5:8888",
			BaggageclaimURL:  "http://5.6.7.9:8899",
			ActiveContainers: 42,
			ResourceTypes: []atc.WorkerResourceType{
				{Type: "some-resource-b", Image: "some-image-b"},
			},
			Platform: "plan9",
			Tags:     []string{"russ", "cox", "was", "here"},
		}

		_, err = database.SaveWorker(infoA, 0)
		Expect(err).NotTo(HaveOccurred())

		savedWorkerB, err := database.SaveWorker(infoB, 0)
		Expect(err).NotTo(HaveOccurred())

		_, err = database.SaveWorker(infoC, 0)
		Expect(err).NotTo(HaveOccurred())

		By("returning one workerinfo by worker id")
		savedWorker, found, err = database.GetWorker(savedWorkerB.ID)
		Expect(err).NotTo(HaveOccurred())
		Expect(found).To(BeTrue())
		Expect(savedWorker.GardenAddr).To(Equal(infoB.GardenAddr))
		Expect(savedWorker.BaggageclaimURL).To(Equal(infoB.BaggageclaimURL))
		Expect(savedWorker.ActiveContainers).To(Equal(infoB.ActiveContainers))
		Expect(savedWorker.ResourceTypes).To(Equal(infoB.ResourceTypes))
		Expect(savedWorker.Platform).To(Equal(infoB.Platform))
		Expect(savedWorker.Tags).To(Equal(infoB.Tags))
		Expect(savedWorker.Name).To(Equal(infoB.Name))

		By("expiring TTLs")
		ttl := 1 * time.Second

		savedWorkerA, err := database.SaveWorker(infoA, ttl)
		Expect(err).NotTo(HaveOccurred())

		workerFound := func() bool {
			_, found, _ = database.GetWorker(savedWorkerA.ID)
			return found
		}

		Consistently(workerFound, ttl/2).Should(BeTrue())
		Eventually(workerFound, 2*ttl).Should(BeFalse())
	})
})

func getWorkerInfos(savedWorkers []db.SavedWorker, err error) []db.WorkerInfo {
	Expect(err).NotTo(HaveOccurred())
	var workerInfos []db.WorkerInfo
	for _, savedWorker := range savedWorkers {
		workerInfos = append(workerInfos, savedWorker.WorkerInfo)
	}
	return workerInfos
}
